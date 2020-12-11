---
layout: page
title: "Atlassian FAQ"
---

## All

* **What levels of information can I put into the various LC (or other) Atlassian instances?**  
See https://atlassian.llnl.gov for a full overview of the security policies that apply to the various instances. In short, the Atlassian instances follow the guidelines for the zones themselves. CZ Atlassian tools are approved for OUO and Export Controlled, but not UCNI or NNPI. RZ is approved for OUO, Export Controlled, UCNI, and NNPI. Only the SCF instance on the classified network is approved for classified information (up to the SRD level).

* **Someone is trying to add me to the permissions for a Confluence Space, Jira Project, or Bitbucket Repo, but I don't show up. What gives?**  
You must first login to the web application before you are known by the system. E.g., [https://lc.llnl.gov/confluence/](https://lc.llnl.gov/confluence/) Having done that, you can be added to access lists and permissions. The information is managed per application, so you need to visit each application separately.

## Bitbucket & Bamboo

* **Can I access mybitbucket.llnl.gov from the CZ?**
Yes, but only if you use git-over-ssh, which requires you to set up SSH keys. In particular, load your ssh public key (e.g., ~/.ssh/id_rsa.pub) into the LLNL Bitbucket (https://mybitbucket.llnl.gov/plugins/servlet/ssh/account/keys/add)

* **Why are the LLNL and CZ BB's separated?**  
The three Bitbucket instances (lc.llnl.gov/bitbucket (CZ), rzlc.llnl.gov/bitbucket (RZ), mybitbucket.llnl.gov (LLNL)) each serve different but overlapping user bases. For example, all LLNL employees have access to mybitbucket, but only LC users would have access to the LC Bitbucket instances. See [https://atlassian.llnl.gov](https://atlassian.llnl.gov) for a full overview of the security policies that apply to the various instances.

* **Can I do CI on the CZ that uses mybitbucket?**  
It is possible to do CI on LC CZ Bamboo using mybitbucket, but we have integrations turned on for all the LC Atlassian tools that makes things much easier if you stay within the LC universe.

* **Can LC's Bitbucket be accessed from outsize the CZ?**  
To access LC Bitbucket via git from outside the LC network, you would need to use ssh-style URLs with git, and you would need to proxy your ssh connection through an LC production host (this is an intentional security policy that we enforce).
See: https://dev.llnl.gov/about/ssh/#proxy-git-over-ssh-through-lc-host

* **Is there a way to sync and/or link the LLNL and CZ bitbucket accounts, i.e. so commits on LLNL BB appear on the CZ BB and vice versa?**  
I do not believe this is possible as connections from LLNL BB would have to be proxyed through LC production machines and I don't think there is any way to configure this in LLNL Bitbucket. Also, the mirroring feature is only available in Bitbucket through the use of a 3rd-party plugin, and only LLNL BB has this plugin installed.

* **Can I mirror between the LLNL BB and the CZ BB?**  
The mirroring would have to be instigated by LLNL BB as it is the only instance that has the mirroring plugin, however I don't believe that plugin could be configured to connect to CZ or RZ BB because of the proxy requirements.  Note that even if mirroring turns out to be physically possible between some combination of CZ, RZ, and LLNL BB, it would still be your responsibility to make sure you were not violating any security policies. For example CZ does not allow UCNI information, but RZ and LLNL do. 
