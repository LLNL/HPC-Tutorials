---
layout: page
title: "Greetings and Test"
#date: 2016-04-08 13:04:00
---

Greetings Doc Day Participants!
===============================

To get started:
---------------

0. Add ssh key to czgitlab - [https://lc.llnl.gov/gitlab/profile/keys](https://lc.llnl.gov/gitlab/profile/keys)
1. Check out the repo:
`git clone ssh://git@czgitlab.llnl.gov:7999/lc/documentation/tutorials.git`
2. Edit content, or add new files 
    - <your-favorite-editor> newfile.**md**
	    *Must include "front-matter" lines at the top of the file:*
		---
		layout: page
		title: "Your Page Title"  (optional)
		---
    - git add file && git commit -m "Message" && git push
3. Pipeline to render HTML and push site to www-lc will run automatically. Check progress at:
[https://lc.llnl.gov/gitlab/lc/documentation/tutorials/-/pipelines](https://lc.llnl.gov/gitlab/lc/documentation/tutorials/-/pipelines)
4. View results at [https://www-lc.llnl.gov/docs/](https://www-lc.llnl.gov/docs/)

*Have fun!*
