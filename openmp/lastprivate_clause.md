---
layout: tutorial_page
title: "OpenMP Directives: Data Scope Attribute Clauses: LASTPRIVATE Clause"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

## Purpose:

The LASTPRIVATE clause combines the behavior of the PRIVATE clause with a copy from the last loop iteration or section to the original variable object.

## Format:

### Fortran	
<pre>
LASTPRIVATE <i>(list)</i>
</pre>

### C/C++	
<pre>
lastprivate <i>(list)</i>
</pre>

## Notes:

The value copied back into the original variable object is obtained from the last (sequentially) iteration or section of the enclosing construct.

For example, the team member which executes the final iteration for a DO section, or the team member which does the last SECTION of a SECTIONS context performs the copy with its own values
