---
layout: page
title: "Point to Point Communication Routines: General Concepts"
release_number: UCRL-MI-133316
author: Blaise Barney, Lawrence Livermore National Laboratory
---

#### First, a Simple Example:

The value of PI can be calculated in various ways. Consider the Monte Carlo method of approximating PI:

* Inscribe a circle with radius $r$ in a square with side length of $2r$
* The area of the circle is $Πr^2$ and the area of the square is $4r^2$
* The ratio of the area of the circle to the area of the square is: 
$Πr^22 / 4r^2 = Π / 4$
* If you randomly generate $N$ points inside the square, approximately 
$N * Π / 4$ of those points ($M$) should fall inside the circle.
* Π is then approximated as: 
$N * Π / 4 = M$
$Π / 4 = M / N$
$Π = 4 * M / N$

Note that increasing the number of points generated improves the approximation.

![pi1](images/pi1.gif)


