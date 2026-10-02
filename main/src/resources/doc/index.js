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

// Briefly shows `msg` near the mouse position of click event `e`, e.g. to confirm that a 'copy'
// button worked. Used by both initCopyLinks and initCopyCode.
function showCopyMsg(e, msg) {
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

            showCopyMsg(e, msg);
        });
    }
}

// Adds a click handler to each 'copy code' button (added to fenced code blocks in doc comments,
// see FencedCodeBlockRenderer in HtmlDocumentor) that copies the plain text of the code block
// next to it to the clipboard.
function initCopyCode() {
    const buttons = document.querySelectorAll(".copy-code");
    for (const button of buttons) {
        button.addEventListener("click", async (e) => {
            const code = button.parentElement.querySelector("pre code");

            let msg;
            try {
                await navigator.clipboard.writeText(code.textContent);
                msg = "Code copied";
            } catch {
                msg = "Failed to copy code ✕";
            }

            showCopyMsg(e, msg);
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
initCopyCode();
initLinks();
