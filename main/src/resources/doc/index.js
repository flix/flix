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

    // Hide menu and search box when navigating somewhere within the page
    if (isAnchor) link.addEventListener("click", () => {
        const menuToggleCheckbox = document.querySelector("#menu-toggle > input");
        menuToggleCheckbox.checked = false;

        const searchBox = document.querySelector("#search-box");
        searchBox.close();
    });
}

function initSearch() {
    const searchButton = document.querySelector("#search-button");
    const searchBox = document.querySelector("#search-box");
    const closeSearchBoxButton = document.querySelector("#close-search-box");
    const input = document.querySelector("#search-box input");
    const resultList = document.querySelector("#search-box .results");

    function openSearchBox() {
        input.value = "";
        updateSearchResults();
        searchBox.showModal();
    }
    searchButton.addEventListener("click", openSearchBox);
    window.addEventListener("keydown", (e) => {
        if (e.key === "/" && searchBox.open === false) {
            e.preventDefault();
            openSearchBox();
        }
    });

    function closeSearchBox() {
        searchBox.close();
    }
    closeSearchBoxButton.addEventListener("click", closeSearchBox);
    searchBox.addEventListener("click", (e) => {
        // Little hack to detect if the backdrop was clicked.
        const backdropClicked = e.target === e.currentTarget;
        if (backdropClicked) {
            closeSearchBox();
        }
    });
    window.addEventListener("keydown", (e) => {
        if (e.key === "Escape") {
            closeSearchBox();
        }
    });


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

                const type = document.createElement("span");
                type.classList.add("type");
                type.textContent = result.type;
                link.append(type);

                link.append(" ");

                const title = document.createElement("span");
                title.classList.add("title");
                title.textContent = result.title;
                link.append(title);

                initLink(link);
                item.append(link);
                resultList.append(item);
            }
        }
    }
    input.addEventListener("input", updateSearchResults);
}

initTheme();
initCopyLinks();
initLinks();
initSearch();
