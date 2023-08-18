const storeKey = "flix-html-docs:use-dark-theme";

const body = document.querySelector("body");
body.classList.remove("no-script");

function setTheme(dark) {
    if (dark) {
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
