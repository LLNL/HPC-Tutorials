---
layout: tutorial_page
title: "OpenMP Directives: Data Scope Attribute Clauses: COPYPRIVATE Clause"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:

* The COPYPRIVATE clause can be used to broadcast values acquired by a single thread directly to all instances of the private variables in the other threads.
* Associated with the SINGLE directive
* See the most recent OpenMP specs document for additional discussion and examples.
 
## Format:

### Fortran	
<pre>
COPYPRIVATE <i>(list)</i>
</pre>
### C/C++	
<pre>
copyprivate  <i>(list)</i>
</pre>
