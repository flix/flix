import { search } from "./search.js";

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
    const menuToggleCheckbox = document.querySelector("#menu-toggle > input");
    const menuLinks = document.querySelectorAll("nav a");

    // Hide sidebar when navigating somewhere within the page
    for (const link of menuLinks) {
        const isAnchor = link.getAttribute("href").startsWith("#");
        if (isAnchor) link.addEventListener("click", () => {
            menuToggleCheckbox.checked = false;
        });
    }
}

function initSearch() {
    const searchButton = document.querySelector("#search-button");
    const searchBox = document.querySelector("#search-box");
    const closeSearchBox = document.querySelector("#close-search-box");
    const input = document.querySelector("#search-box input");
    const resultList = document.querySelector("#search-box .results");

    async function updateSearchResults() {
        const phrase = input.value;
        resultList.innerHTML = "Searching...";
        const results = await search(phrase);

        if (input.value !== phrase) {
            // The results are outdated
            return;
        }

        resultList.innerHTML = "";
        if (results.length === 0) {
            resultList.innerHTML = "No results found";
        } else {
            for (const result of results) {
                const item = document.createElement("li");
                const link = document.createElement("a");
                link.href = result.url;
                link.innerText = result.title;
                item.appendChild(link);
                resultList.appendChild(item);
            }
        }
    }

    searchButton.addEventListener("click", () => {
        input.value = "";
        updateSearchResults();
        searchBox.showModal();
    });
    closeSearchBox.addEventListener("click", () => {
        searchBox.close();
    });
    input.addEventListener("input", updateSearchResults);
}

initTheme();
initCopyLinks();
initMobileInteractions();
initSearch();
