---
layout: default
title: "Getting Started with Jekyll and GitLab Pages"
---

Greetings Doc Day Participants!
===============================

To get started:
---------------

0. Add ssh key to czgitlab - [https://lc.llnl.gov/gitlab/profile/keys](https://lc.llnl.gov/gitlab/profile/keys)
1. Check out the repo:
`git clone ssh://git@czgitlab.llnl.gov:7999/lc/documentation/tutorials.git`
2. Edit content, or add new files
    - `cd tutorials/pages/`
    - `your-favorite-editor newfile.md`  (see tip below for required lines in .md files)
    - `git add file && git commit -m "Message" && git push`
3. The pipeline to render HTML and push site to www-lc will run automatically. Check progress at:
[https://lc.llnl.gov/gitlab/lc/documentation/tutorials/-/pipelines](https://lc.llnl.gov/gitlab/lc/documentation/tutorials/-/pipelines)
4. Once pipeline has finished, view results at [https://www-lc.llnl.gov/docs/pages/](https://www-lc.llnl.gov/docs/pages/)

Tips:
-----
* [Markdown Syntax Review](https://www.markdownguide.org/basic-syntax/)
* The top of your markdown (.md) files **must** have these lines in order to get rendered into html by Jekyll:
```
---
layout: default
title: "Your Page Title"  (optional)
---
```
*Have fun!*
