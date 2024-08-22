/**
 * @returns {Promise<Document[]>}
 */
async function initSearchIndex() {
    const res = await fetch("sitemap.json");
    const sitemap = await res.json();

    const parser = new DOMParser();
    const documents = await Promise.all(sitemap.map(async (filename) => {
        const res = await fetch(filename);
        const text = await res.text();
        const loadedDoc = parser.parseFromString(text, "text/html");
        return loadedDoc;
    }));

    return documents;
}
const indexPromise = initSearchIndex();

/**
 * Search the documentation for the given `phrase`.
 * Reutrns a list of results ordered by priority, with the highest priority first.
 *
 * @param {string} phrase
 * @returns {Promise<{ url: string, type: string, title: string }[]>}
 */
export async function search(phrase) {
    const priorityLevels = {
        exactMatchInTitle: 4,
        pageWithPartialMatchInTitle: 3,
        boxWithPartialMatchInTitle: 2,
        matchInDescription: 1,
    };

    phrase = phrase.toLowerCase().trim();

    const index = await indexPromise;

    /** @type {{ url: string, type: string, title: string, priority: number }[]} */
    const results = [];

    for (const document of index) {
        const pageTitle = document.querySelector("h1").textContent;
        const pageTitleLower = pageTitle.toLowerCase();
        const pageFilename = document.querySelector("base").href;

        const pageBox = document.querySelector("#main-box");
        const pageType = pageBox?.querySelector(".keyword")?.textContent ?? "mod";
        const pageDescription = pageBox?.querySelector(".doc")?.textContent;
        const pageDescriptionLower = pageDescription?.toLowerCase();

        if (pageTitleLower === phrase) {
            results.push({
                url: pageFilename,
                type: pageType,
                title: pageTitle,
                priority: priorityLevels.exactMatchInTitle,
            });
        } else if (pageTitleLower.includes(phrase)) {
            results.push({
                url: pageFilename,
                type: pageType,
                title: pageTitle,
                priority: priorityLevels.pageWithPartialMatchInTitle,
            });
        } else if (pageDescriptionLower?.includes(phrase) ?? false) {
            results.push({
                url: pageFilename,
                type: pageType,
                title: pageTitle,
                priority: priorityLevels.matchInDescription,
            });
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

            const type = box.querySelector(".keyword").textContent;

            const description = box.querySelector(".doc")?.textContent?.toLowerCase();
            const descriptionLower = description?.toLowerCase();

            if (titleLower === phrase) {
                results.push({
                    url,
                    type,
                    title,
                    priority: priorityLevels.exactMatchInTitle,
                });
            } else if (titleLower.includes(phrase)) {
                results.push({
                    url,
                    type,
                    title,
                    priority: priorityLevels.boxWithPartialMatchInTitle,
                });
            } else if (descriptionLower?.includes(phrase) ?? false) {
                results.push({
                    url,
                    type,
                    title,
                    priority: priorityLevels.matchInDescription
                });
            }
        }
    }

    return results.sort((a, b) => b.priority - a.priority);
}
