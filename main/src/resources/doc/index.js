function initTheme() {
    const storeKey = "flix-html-docs:theme";

    const root = document.documentElement;
    const menu = document.querySelector("#theme-menu");
    const items = Array.from(menu.querySelectorAll("[role='menuitemradio']"));

    // Storage can be unavailable, e.g. in a private window; the choice then lasts for the page.
    function store(theme) {
        try {
            if (theme === null) {
                localStorage.removeItem(storeKey);
            } else {
                localStorage.setItem(storeKey, theme);
            }
        } catch {}
    }

    function mark(theme) {
        for (const item of items) {
            item.setAttribute("aria-checked", item.value === theme);
        }
    }

    // `system` is no theme: with the attribute unset, the stylesheet follows the system.
    function select(theme) {
        if (theme === "system") {
            delete root.dataset.theme;
            store(null);
        } else {
            root.dataset.theme = theme;
            store(theme);
        }
        mark(theme);
    }

    // The head script has applied the stored theme before first paint; mark it in the menu. A
    // stored theme the menu does not know counts as none.
    const current = root.dataset.theme;
    mark(items.some((item) => item.value === current) ? current : "system");

    for (const item of items) {
        item.addEventListener("click", () => {
            select(item.value);
            menu.hidePopover();
        });
    }

    // The browser opens and closes the popover; the menu pattern adds the focus on the chosen
    // theme when it opens, and the arrow keys between the themes.
    menu.addEventListener("toggle", (e) => {
        if (e.newState === "open") {
            const checked = items.find((item) => item.getAttribute("aria-checked") === "true");
            (checked ?? items[0]).focus();
        }
    });
    menu.addEventListener("keydown", (e) => {
        const i = items.indexOf(document.activeElement);
        if (i < 0) return;
        const moves = { ArrowDown: i + 1, ArrowUp: i - 1, Home: 0, End: items.length - 1 };
        const j = moves[e.key];
        if (j === undefined) return;
        e.preventDefault();
        items[(j + items.length) % items.length].focus();
    });
}

function initCopyLinks() {
    const links = document.querySelectorAll(".copy-link");
    for (const link of links) {
        link.addEventListener("click", async (e) => {
            e.preventDefault();

            let msg;
            try {
                await navigator.clipboard.writeText(link.href);
                msg = "Link copied";
            } catch {
                msg = "Failed to copy link ✕";
            }

            const msgNode = document.createElement("div");
            msgNode.classList.add("copy-link-msg");
            msgNode.textContent = msg;
            msgNode.style.position = "absolute";
            msgNode.style.top = `${e.clientY}px`;
            msgNode.style.left = `${e.clientX}px`;
            document.body.append(msgNode);

            msgNode.addEventListener("animationend", () => {
                msgNode.remove();
            });
        });
    }
}

function initLinks() {
    const links = document.querySelectorAll("a");
    links.forEach(initLink);
}
function initLink(link) {
    const isAnchor = link.getAttribute("href").includes("#");

    // Hide menu when navigating somewhere within the page
    if (isAnchor) link.addEventListener("click", () => {
        const menuToggleCheckbox = document.querySelector("#menu-toggle > input");
        menuToggleCheckbox.checked = false;
    });
}

initTheme();
initCopyLinks();
initLinks();
