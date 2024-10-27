# tinyc

**a software 1.0 compiler: C89/90 -> RV32G**

```
tinyc snapshots the happy paths, and smoked against gcc with egos2, xv6, git, sqlite.
tinyc is optimized 20/80-wise to beat gcc -O1 on dhrystone and embench.
```

<!-- Future reading:
- ECOOP 2003: The verifying compiler: A grand challenge for computing research (Hoare)
- FM 2006: Formal Verification of a C Compiler Front-end (Blazy, Daygaye, Leroy)
- POPL 2006: Formal certification of a compiler back-end, or: programming a compiler with a proof assistant
  discuss: https://www.cs.cornell.edu/courses/cs6120/2019fa/blog/comp-cert/
- PLDI 2011: Finding and Understanding Bugs in C Compilers (Yang, Chen, Eide, Regehr)
  discuss: https://www.cs.cornell.edu/courses/cs6120/2019fa/blog/bug-finding/
- ICFP 2019: The Next 700 Compiler Correctness Theorems (Patterson, Ahmed)
  discuss: https://priyasrikumar.com/next700ccc.pdf

  regehr
- https://users.ece.cmu.edu/~ganger/712.fall02/papers/p761-thompson.pdf
- https://www.cs.cmu.edu/~crary/819-f09/Landin66.pdf
- https://www.khoury.northeastern.edu/home/amal/papers/next700ccc.pdf
- https://www.williamjbowman.com/blog/2023/06/15/what-is-a-model/
- https://www.williamjbowman.com/blog/2023/06/07/what-is-syntax/
- https://www.williamjbowman.com/blog/2023/03/24/what-is-logical-relations/
- https://www.williamjbowman.com/blog/2017/03/24/what-even-is-compiler-correctness/
- https://www.google.com/search?q=csmith+compiler+reddit&sca_esv=114fca881d0d3d09&sxsrf=ACQVn09leOSX0UC5Fwz_rrCpOjAwR3sPZA%3A1712357977633&ei=WYIQZoqlJp7E0PEPof-ugA4&ved=0ahUKEwjKtfDHlqyFAxUeIjQIHaG_C-AQ4dUDCBA&uact=5&oq=csmith+compiler+reddit&gs_lp=Egxnd3Mtd2l6LXNlcnAiFmNzbWl0aCBjb21waWxlciByZWRkaXQyCBAhGKABGMMEMggQIRigARjDBEiVFlDTBVjaFHAEeACQAQGYAY4BoAGJCqoBBDEyLjO4AQPIAQD4AQGYAgugAuMFwgIIEAAYgAQYogTCAgoQIRgKGKABGMMEmAMAiAYBkgcDOS4yoAeVMQ&sclient=gws-wiz-serp#ip=1
- https://www.google.com/search?q=yarpgen+reddit&oq=yarpgen+reddit&gs_lcrp=EgZjaHJvbWUyCwgAEEUYChg5GKABMgkIARAhGAoYoAEyCQgCECEYChigATIJCAMQIRgKGKAB0gEIMTIwNWoxajeoAgCwAgA&sourceid=chrome&ie=UTF-8
- https://blog.sigplan.org/2021/01/14/finding-bugs-in-c-and-c-compilers-using-yarpgen/
- https://www.cs.cornell.edu/~asampson/blog/reduction.html
https://www.youtube.com/watch?v=WEwEPAF53n4&list=PL_R5A0lGi1ADOH1rT8c5ysZaU14O41vxO&index=4
https://www.flux.utah.edu/profile/jxyang -->