# AlgoMonad

## Author

Karl Wang

## Idea

AlgoMoond is a haskell TUI that allows user to download LeetCode questions through the TUI to their local machines, edit them in their favorite editors, ansubmit them through the TUI.

The motivation to create this program is that LeetCode only allows the user to edit and submit the code on their website. However, programmers tend to want to edit code in their favorite editors instead of in the browser. Hence, I would like to create a interface that allows the user to download to local machine and submit from local machine.

## Installation

Clone the repository and run `stack install` to install the TUI. To run the test cases, run `stack test`

## Guide

### Login Credientials

AlgoMonad needs to use the cookie credientials from leetcode in order to acquire user leetcode information. If you are on MacOS, you can either acquire the credientials manually or automatically. If you are on other platforms, currently you can only acquire the credientials manually.

#### Manual

If not already login, login to leetcode account on [leetcode.com](https://leetcode.com/) on a Chromium browser.

After that, go to [chrome://settings/cookies/detail?site=leetcode.com](chrome://settings/cookies/detail?site=leetcode.com) to copy the value of **LEETCODE_SESSION** and **csrftoken** under _Content_

Then create the file `~/.algomonad.yaml` and write the file as followed:

```yaml
LEETCODE_SESSION: YOUR_LEETCODE_SESSION_VALUE
csrftoken: YOUR_CSRFTOKEN_VALUE
```

#### Automatic

Currently, the automatic cookie extraction only works on MacOS with **Chrome** browser.

If not already login, login to leetcode account on [leetcode.com](https://leetcode.com/) on Google Chrome browser.

Then run `algomonad login` on the shell. This should create a file at `~/.algomonad.yaml` with two keys, **LEETCODE_SESSION** and **csrftoken**.

#### Example

An example of the config file should look like

```yaml
LEETCODE_SESSION: eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJfYXV0aF91c2VyX2lkIjoiMjAzNDM3MCIsIl9hdXRoX3VzZXJfYmFja2VuZCI6ImFsbGF1dGguYWNjb3VudC5hdXRoX2JhY2tlbmRzLkF1dGhlbnRpY2F0aW9uQmFja2VuZCaaksi9hdXRoX3VzqXJfaGFzaCI6IjM3ZDgxYzI0NTFkZWIxOTAzZDk2ZTMzMTY1YzIwMzgwMTc3YTYxMDciLCJpZCI6MjAzNDM3MCwiZW1haWwiOiJrYXdhbmdAdWNzZC5lZHUiLCJ1c2VybmFtZSI6Imthd2FuZyIsInVzZXJfc2x1ZyI6Imthd2FuZyIsImF2YXRhciI6Imh0dHBzOi8vYXNzZXRzLmxlZXRjb2RlLmNvbS91c2Vycy9rYXdhbmcvYXZhdGFyXzE1NjEyMjg5ODMucG5nIiwiqiwkmVzaGVkX2F0IjoxNjM5MDE0NDkxLCJpcCI6Ijc1LjgwLjEwOC4xNzIiLCJpZGVudGl0eSI6IjAzMjE0NWI4NDc3ZDZiM2ZiMzRlMGU1OTU0YjU5YjI0Iiwic2Vzc2lvbl9pZCI6MTU0ODI2NjZ9.K-jh_WU-KOknhPRYyh4OiRs8vqGnRc1JU8tmILGoHiM
csrftoken: W43GFtqFER8nADnf9apPmfUTYqrkPRz1XN6V712GAJWuE1RxUThv9QlQyGlb5Prh
```

### Usage

With the credientials acquired, now run `algomonad` on the shell. You should be greeted with the following interface

![List](./assets/List.png)

AlgoMonad uses vim keybindings. This means that use _k/j_ to move up and down. _l_ to select question. _h_ to go back to the problem list.
_Ctrl-d_ to move half page down and _Ctrl-u_ to move half page up.

If a language is selected, use _enter_ to download the question. The question will be downloaded to the current directory.

When in problem list, use _/_ to do a case insensitive search by title, with _n_ to search the next occurance. Use _=_ to search by problem id.

Use _q_ to quit.

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

- [x] Acquire user token automatically through browser cookies.
- [x] Browse LeetCode questions.
- [x] Download and write LeetCode questions to local files.
- [ ] Submit local files to LeetCode and evaluate result.

## Inspirations

- [leetcode-cli](https://github.com/skygragon/leetcode-cli): Main source of inspiration. However, this is just a pure cli instead of a TUI. Implemented in javascript
- [vscode-leetcode](https://github.com/LeetCode-OpenSource/vscode-leetcode): A GUI for LeetCode. However, only available on VSCode. Implemented in javascript.
