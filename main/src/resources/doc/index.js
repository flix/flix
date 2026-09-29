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

/**
 * Returns `true` if the user is currently typing into a text input, a text area, or a
 * `contenteditable` element, in which case keyboard shortcuts should not be triggered.
 */
function isTypingInTextField() {
    const el = document.activeElement;
    if (el === null) {
        return false;
    }
    if (el.isContentEditable) {
        return true;
    }

    const tag = el.tagName;
    if (tag === "TEXTAREA") {
        return true;
    }
    if (tag === "INPUT") {
        // Inputs like checkboxes, radios, and buttons don't accept text, so shortcuts should
        // still work while one of those happens to be focused.
        const nonTextTypes = new Set([
            "checkbox", "radio", "button", "submit", "reset", "range", "color", "file", "image",
        ]);
        const type = (el.getAttribute("type") || "text").toLowerCase();
        return !nonTextTypes.has(type);
    }

    return false;
}

/**
 * Finds every collapsable "Instances" section on the current page, i.e. every
 * `<details class='subsection'>` whose summary reads "Instances".
 */
function findInstancesSections() {
    const allSubsections = document.querySelectorAll("details.subsection");
    return Array.from(allSubsections).filter((details) => {
        const summary = details.querySelector(":scope > summary");
        return summary !== null && summary.textContent.trim() === "Instances";
    });
}

/**
 * Wires up the "Instances" sections found on the page so that:
 * - Their open/closed state is remembered per page (keyed by the enclosing type/trait name) and
 *   restored on future visits, using the same `localStorage` pattern as the theme preference.
 * - They can all be expanded or collapsed together, via `toggleAll()`.
 *
 * Returns `null` if there are no "Instances" sections on the page.
 */
function initInstances() {
    const storeKey = "flix-html-docs:open-instances";

    const sections = findInstancesSections();
    if (sections.length === 0) {
        return null;
    }

    // Identify the page by the name of the type/trait it documents, so that the same name on
    // different pages doesn't collide and the same page keeps its identity across reloads.
    const heading = document.querySelector("main h1");
    const pageId = heading !== null ? heading.textContent.trim() : document.title;

    function readStore() {
        try {
            const raw = localStorage.getItem(storeKey);
            return raw === null ? {} : JSON.parse(raw);
        } catch {
            return {};
        }
    }

    function writeStore(store) {
        try {
            localStorage.setItem(storeKey, JSON.stringify(store));
        } catch {
            // Ignore storage errors, e.g. private browsing or a full quota.
        }
    }

    sections.forEach((details, index) => {
        // There is normally only a single "Instances" section per page, but the index keeps
        // the key unique in case a page ever has more than one.
        const key = `${pageId}#${index}`;

        const store = readStore();
        if (Object.prototype.hasOwnProperty.call(store, key)) {
            details.open = store[key];
        }

        details.addEventListener("toggle", () => {
            const store = readStore();
            store[key] = details.open;
            writeStore(store);
        });
    });

    function toggleAll() {
        // If any section is closed, open all of them; otherwise close all of them.
        const shouldOpen = sections.some((details) => !details.open);

        const store = readStore();
        sections.forEach((details, index) => {
            details.open = shouldOpen;
            store[`${pageId}#${index}`] = shouldOpen;
        });
        writeStore(store);
    }

    return {toggleAll};
}

const KEYBOARD_SHORTCUTS = [
    {keys: "e", desc: "Expand or collapse all Instances sections"},
    {keys: "?", desc: "Show or hide this help panel"},
    {keys: "Esc", desc: "Close this help panel"},
];

/**
 * Builds (lazily) and controls the keyboard shortcuts help overlay.
 */
function initKeyboardHelp() {
    let overlay = null;

    function build() {
        const backdrop = document.createElement("div");
        backdrop.id = "keyboard-shortcuts-help";
        backdrop.setAttribute("role", "dialog");
        backdrop.setAttribute("aria-modal", "true");
        backdrop.setAttribute("aria-label", "Keyboard shortcuts");
        Object.assign(backdrop.style, {
            position: "fixed",
            inset: "0",
            display: "none",
            alignItems: "center",
            justifyContent: "center",
            backgroundColor: "rgba(0, 0, 0, 0.4)",
            zIndex: "1000",
        });
        backdrop.addEventListener("click", (e) => {
            if (e.target === backdrop) {
                hide();
            }
        });

        const panel = document.createElement("div");
        Object.assign(panel.style, {
            position: "relative",
            backgroundColor: "var(--bg-color-1)",
            color: "var(--text-color)",
            border: "var(--border)",
            borderRadius: "0.4rem",
            boxShadow: "0 0 20px 0 var(--shadow-color)",
            padding: "1.25rem 1.5rem",
            maxWidth: "24rem",
            width: "90%",
            fontFamily: "'Inter', sans-serif",
            boxSizing: "border-box",
        });

        const closeButton = document.createElement("button");
        closeButton.textContent = "✕";
        closeButton.setAttribute("aria-label", "Close");
        Object.assign(closeButton.style, {
            position: "absolute",
            top: "0.5rem",
            right: "0.5rem",
            border: "none",
            background: "none",
            color: "var(--faded-text-color)",
            cursor: "pointer",
            fontSize: "0.9rem",
        });
        closeButton.addEventListener("click", hide);
        panel.append(closeButton);

        const title = document.createElement("h2");
        title.textContent = "Keyboard Shortcuts";
        title.style.marginTop = "0";
        panel.append(title);

        const list = document.createElement("dl");
        Object.assign(list.style, {
            display: "grid",
            gridTemplateColumns: "auto 1fr",
            gap: "0.5rem 0.9rem",
            margin: "0",
            alignItems: "center",
        });
        for (const {keys, desc} of KEYBOARD_SHORTCUTS) {
            const dt = document.createElement("dt");
            const kbd = document.createElement("kbd");
            kbd.textContent = keys;
            Object.assign(kbd.style, {
                border: "var(--border)",
                borderRadius: "0.25rem",
                padding: "0.1rem 0.45rem",
                backgroundColor: "var(--bg-color-3)",
                fontFamily: "monospace",
            });
            dt.append(kbd);

            const dd = document.createElement("dd");
            dd.textContent = desc;
            dd.style.margin = "0";

            list.append(dt, dd);
        }
        panel.append(list);

        backdrop.append(panel);
        document.body.append(backdrop);
        return backdrop;
    }

    function show() {
        if (overlay === null) {
            overlay = build();
        }
        overlay.style.display = "flex";
    }

    function hide() {
        if (overlay !== null) {
            overlay.style.display = "none";
        }
    }

    function isVisible() {
        return overlay !== null && overlay.style.display !== "none";
    }

    return {show, hide, isVisible};
}

/**
 * Wires up all keyboard shortcuts: the `?` help overlay and the "expand/collapse all Instances"
 * shortcut.
 */
function initKeyboardShortcuts() {
    const help = initKeyboardHelp();
    const instances = initInstances();

    document.addEventListener("keydown", (e) => {
        if (e.defaultPrevented || e.ctrlKey || e.metaKey || e.altKey) {
            return;
        }

        if (e.key === "Escape") {
            if (help.isVisible()) {
                e.preventDefault();
                help.hide();
            }
            return;
        }

        if (help.isVisible()) {
            // While the help panel is open, only let `?` (to close it) through.
            if (e.key === "?") {
                e.preventDefault();
                help.hide();
            }
            return;
        }

        // Don't hijack normal typing if focus happens to be in a text field.
        if (isTypingInTextField()) {
            return;
        }

        if (e.key === "?") {
            e.preventDefault();
            help.show();
        } else if ((e.key === "e" || e.key === "E") && instances !== null) {
            e.preventDefault();
            instances.toggleAll();
        }
    });
}

initTheme();
initCopyLinks();
initLinks();
initKeyboardShortcuts();
