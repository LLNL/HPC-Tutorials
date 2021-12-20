---
layout: tutorial_page
title: "OpenMP Directives: Directive Binding and Nesting Rules"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
tutorial: "OpenMP"
---

* This section is provided mainly as a quick reference on rules which govern OpenMP directives and binding. Users should consult their implementation documentation and the OpenMP standard for other rules and restrictions.

* Unless indicated otherwise, rules apply to both Fortran and C/C++ OpenMP implementations.

* Note: the Fortran API also defines a number of Data Environment rules. Those have not been reproduced here.

## Directive Binding:

* The DO/for, SECTIONS, SINGLE, MASTER and BARRIER directives bind to the dynamically enclosing PARALLEL, if one exists. If no parallel region is currently being executed, the directives have no effect.
* The ORDERED directive binds to the dynamically enclosing DO/for.
* The ATOMIC directive enforces exclusive access with respect to ATOMIC directives in all threads, not just the current team.
* The CRITICAL directive enforces exclusive access with respect to CRITICAL directives in all threads, not just the current team.
* A directive can never bind to any directive outside the closest enclosing PARALLEL.

## Directive Nesting:

* A worksharing region may not be closely nested inside a worksharing, explicit task, critical, ordered, atomic, or master region.
* A barrier region may not be closely nested inside a worksharing, explicit task, critical, ordered, atomic, or master region.
* A master region may not be closely nested inside a worksharing, atomic, or explicit task region.
* An ordered region may not be closely nested inside a critical, atomic, or explicit task region.
* An ordered region must be closely nested inside a loop region (or parallel loop region) with an ordered clause.
* A critical region may not be nested (closely or otherwise) inside a critical region with the same name. Note that this restriction is not sufficient to prevent deadlock.
* parallel, flush, critical, atomic, taskyield, and explicit task regions may not be closely nested inside an atomic region.
