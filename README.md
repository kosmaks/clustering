Experimenting with AI
=====================

Clustered cloud of points

How to compile
==============

The only dependency is gnuplot, so no need to use cabal to install.

```bash
$ make
```

How to run
==========

Pass the input through stdin. The input coordinates should be formatted as line separated "x y _" points.

Using runhaskell:

```bash
$ cat input.txt | runhaskell clust.hs 47 Complete
```

Compiled version:

```bash
$ cat input.txt | ./clust 47 Complete
```

Screenshot
==========

![alt tag](https://raw.github.com/kosmaks/clustering/master/screenshot.png)




Лаба по AI
==========

Кластеризовать облако точек.

Как скомпилировать
==================

Из зависимостей только gnuplot, поэтому без кабалов обошлось.

```bash
$ make # лол
```


Как запустить
=============

В stdin подаются координаты в формате "x y _" по одной на строке.

Через runhaskell:

```bash
$ cat input.txt | runhaskell clust.hs 47 Complete
```

Скомпилированную версию:

```bash
$ cat input.txt | ./clust 47 Complete
```

Пример работы
=============

![alt tag](https://raw.github.com/kosmaks/clustering/master/screenshot.png)
