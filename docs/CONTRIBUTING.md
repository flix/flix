# Contributor Guide

Hello and thank you for your interest in working on Flix! :-)

## The Process

We suggest that you proceed by following these steps:

1. Pick an issue on GitHub that you are interested in. 
   - It could be adding a new feature, fixing a bug in the compiler, and so forth.

2. Express interest in the issue on GitHub.
    - We try to ensure that multiple people are not working on the same issue.
 
3. We discuss the issue and how best to fix it.
    - The idea is to come up with an implementation plan and to hash out any design choices.

4. Fork the Flix repository to your own account on GitHub.

5. Clone the forked repository to your own machine.

6. Make the required changes, add test cases, documentation, and so forth.

7. Push the local changes to your GitHub repository and create a new pull request (PR).
   - See below for instructions on how to make a good pull request.

8. GitHub automatically compiles and tests the PR. Be sure to check the results!

9. We perform a code review and iterate on the implementation.
   - You should expect that most PRs will require multiple iterations.

**Note:** You must agree to release your contributions under the Apache 2.0 license. You affirm this by adding yourself to AUTHORS.md.

## Pull Requests

- Pull requests should use [Semantic Commit Messages](https://gist.github.com/joshbuchea/6f47e86d2510bce28f8e7f42ae84c716).
    - We use the tags: `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`.
    - The summary should be a single clear sentence (all lower case except for names).
- Pull requests should be a small as possible.
    - An ideal pull request is between one and a few hundred lines of code.
    - A pull request should focus on a single change.
    - Most features are broken down into multiple pull requests.
- When you create a pull request, click `Files changed` to ensure that everything looks as you expect.
    - The easier a pull request is to understand the faster it will be merged. A win-win.
- If the pull request fixes a bug the summary should end with (fixes #9999).
- If the pull request is related to a ticket the summary should end with (related to #9999).

## Code Quality

We are interested in correct, well-documented, and well-tested code. 

We expect all pull requests to be maintainable and follow established style.

Please have a look at [STYLE.md](./STYLE.md) for more information on how to structure your code.
