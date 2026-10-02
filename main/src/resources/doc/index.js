function initTheme() {
    const storeKey = "flix-html-docs:use-dark-theme";

    const root = document.querySelector(":root");

    const body = document.querySelector("body");
    body.classList.remove("no-script");

    function setTheme(useDarkTheme) {
        if (useDarkTheme) {
            root.classList.remove("light");
            root.classList.add("dark");
        } else {
            root.classList.remove("dark");
            root.classList.add("light");
        }
    }

    let dark;
    const stored = localStorage.getItem(storeKey);
    if (stored === null) {
        // User has not stored any preference. Check if the browser prefers a dark theme.
        dark = window.matchMedia("(prefers-color-scheme: dark)").matches;
    } else {
        // User has set a preference. Use it.
        dark = stored === "true";
    }
    setTheme(dark);

    const toggle = document.querySelector("#theme-toggle");
    toggle.addEventListener("click", () => {
        dark = !dark;
        setTheme(dark);
        localStorage.setItem(storeKey, dark);
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

function initSourceLines() {
    const code = document.querySelector(".source-code");
    if (code === null) return;

    // Marks the lines named by the fragment, e.g. `#L10` or `#L10-L20`, and scrolls to the first.
    function selectLines() {
        for (const line of code.querySelectorAll(".selected")) {
            line.classList.remove("selected");
        }

        const match = /^#L(\d+)(?:-L(\d+))?$/.exec(window.location.hash);
        if (match === null) return;

        const start = Number(match[1]);
        const end = match[2] === undefined ? start : Number(match[2]);
        for (let i = start; i <= end; i++) {
            const line = document.getElementById(`L${i}`);
            // The range is taken from the URL, so it may run past the end of the file.
            if (line === null) break;
            line.classList.add("selected");
        }

        const first = document.getElementById(`L${start}`);
        if (first !== null) first.scrollIntoView();
    }

    selectLines();
    window.addEventListener("hashchange", selectLines);
}

initTheme();
initCopyLinks();
initLinks();
initSourceLines();
