function initTheme() {
    const storeKey = "flix-html-docs:use-dark-theme";

    const body = document.querySelector("body");
    body.classList.remove("no-script");

    function setTheme(useDarkTheme) {
        if (useDarkTheme) {
            body.classList.remove("light");
            body.classList.add("dark");
        } else {
            body.classList.remove("dark");
            body.classList.add("light");
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
    toggle.removeAttribute("disabled");
    toggle.addEventListener("click", () => {
        dark = !dark;
        setTheme(dark);
        localStorage.setItem(storeKey, dark);
    });
}

function initCopyLinks() {
    const links = document.querySelectorAll(".copy-link");
    for (const link of links) {
        link.setAttribute("title", "Copy link");
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
            document.body.appendChild(msgNode);

            msgNode.addEventListener("animationend", () => {
                msgNode.remove();
            });
        });
    }
}

function initMobileInteractions() {
    const menuToggle = document.querySelector("#menu-toggle");
    const menuLinks = document.querySelectorAll("nav a");
    const main = document.querySelector("main");

    function onToggle() {
        main.style.pointerEvents = menuToggle.checked ? "none" : "all";
    }

    menuToggle.addEventListener("change", onToggle);

    for (const link of menuLinks) {
        const isAnchor = link.getAttribute("href").startsWith("#");
        if (isAnchor) link.addEventListener("click", () => {
            menuToggle.checked = false;

            // This is needed because the change event is not fired when 
            // the toggle is unchecked programmatically.
            onToggle();
        });
    }
}

initTheme();
initCopyLinks();
initMobileInteractions();
