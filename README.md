# AlgoMonad

## Author

Karl Wang

## Idea

AlgoMoond is a haskell TUI that allows user to download LeetCode questions through the TUI to their local machines, edit them in their favorite editors, ansubmit them through the TUI.

The motivation to create this program is that LeetCode only allows the user to edit and submit the code on their website. However, programmers tend to want to edit code in their favorite editors instead of in the browser. Hence, I would like to create a interface that allows the user to download to local machine and submit from local machine.

## Architecture

The architecture is split into two parts, frontend and backend. The frontend mainly deals with Brick library to handle user events and updating the states. The backend is responsible for network requests to get problems and user information.

Some of the key components are the following:

- **UserInfo**: get the username and whether the user has leetcode premium.
- **Problem**: get the list of problems and their statistics.
- **ProblemDetail**: get the code and writeup of the problem.

## Challenges

Some of the challenges are the following:

- Displaying cursor and scroll through problems. Initially, I was using external library **Cursor** to list the problems and show the cursor. However, with around 2000 questions, using this third part library is extremely slow and inefficient. Then I discover that Brick has a builtin component to display the cursor and the list. Using the component, I was able to successfully show the cursor in the list.

- Getting user token from leetcode requires me to open the local sqllite cookie file and query the token, which I have yet explore. Currently the token is hardcoded. If there is not enough time, then I may need the user to hardcode their token into a config file.

## Expectation

I am first finishing the listing questions functionality, then downloading specific quesiton. After that, I am going to try to acquire the user token from the cookies. In the end, I will try to implement the submission functionality. I am confident that I can finish the first two. However, I might get to the submission functionality by deadline. If that is the case, then the submission functionality will be abandoned.

## Libraries

Here is a list of libraries needed for this program

- [req](https://hackage.haskell.org/package/req): use to make request
- [brick](https://hackage.haskell.org/package/brick): use to make TUI

## Goals

- [ ] Acquire user token automatically through browser cookies.
- [x] Browse LeetCode questions.
- [ ] Download and write LeetCode questions to local files.
- [ ] Submit local files to LeetCode and evaluate result.

## Inspirations

- [leetcode-cli](https://github.com/skygragon/leetcode-cli): Main source of inspiration. However, this is just a pure cli instead of a TUI. Implemented in javascript
- [vscode-leetcode](https://github.com/LeetCode-OpenSource/vscode-leetcode): A GUI for LeetCode. However, only available on VSCode. Implemented in javascript.
