/**
 * @returns {Promise<({ filename: string } & Document)[]>}
 */
async function initSearchIndex() {
    const res = await fetch("sitemap.json");
    const sitemap = await res.json();

    const parser = new DOMParser();
    const documents = await Promise.all(sitemap.map(async (p) => {
        const res = await fetch(p);
        const text = await res.text();
        const document = parser.parseFromString(text, "text/html");
        document.filename = p;
        return document;
    }));

    return documents;
}
const indexPromise = initSearchIndex();

/**
 * Search the documentation for the given `phrase`.
 * Reutrns a list of results ordered by priority, with the highest priority first.
 *
 * @param {string} phrase
 * @returns {Promise<{ url: string, title: string }[]>}
 */
export async function search(phrase) {
    // Priority is given as follows:
    // 4. Exact match in title
    // 3. Page with partial match in title
    // 2. Box with partial match in title
    // 1. Match in description

    phrase = phrase.toLowerCase();

    const index = await indexPromise;

    /** @type {{ url: string, title: string, priority: number }[]} */
    const results = [];

    for (const document of index) {
        const pageTitle = document.querySelector("h1").textContent;
        const pageTitleLower = pageTitle.toLowerCase();
        const pageFilename = document.filename;
        if (pageTitleLower === phrase) {
            results.push({ url: pageFilename, title: pageTitle, priority: 4 });
        } else if (pageTitleLower.includes(phrase)) {
            results.push({ url: pageFilename, title: pageTitle, priority: 3 });
        }

        const boxes = document.querySelectorAll(".box");
        for (const box of boxes) {
            const url = box.querySelector(".copy-link")?.href;
            if (url === undefined) {
                // Boxes with no link can be ignored.
                continue;
            }

            const boxTitle = box.querySelector(".name").textContent;
            const title = `${pageTitle}.${boxTitle}`;
            const titleLower = title.toLowerCase();

            const description = box.querySelector(".doc")?.textContent?.toLowerCase();
            const descriptionLower = description?.toLowerCase();

            if (titleLower === phrase) {
                results.push({ url, title, priority: 4 });
            } else if (titleLower.includes(phrase)) {
                results.push({ url, title, priority: 2 });
            } else if (descriptionLower?.includes(phrase) ?? false) {
                results.push({ url, title, priority: 1 });
            }
        }
    }

    return results.sort((a, b) => b.priority - a.priority);
}
