// The script of a source page, see HtmlHighlighter.

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

initSourceLines();
