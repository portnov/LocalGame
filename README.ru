LocalGame
=========

Это местная карточная игра. Вероятно, произошла от канасты (http://ru.wikipedia.org/wiki/Канаста), но, может быть, просто имеет с канастой общего предка или является полностью самостоятельной игрой, а правила похожи случайно.

Для игры используется колода в 54 карты. Количество игроков — от двух до... многих, лишь бы карт хватило.

Правила следующие.
Целью игры для каждого игрока является сделать как можно больше *выкладок* (meld). Выкладка — это три или больше карты, выкладываемые на стол лицом вверх, удовлетворяющих одному из условий:

* Все карты одной масти, а значения идут подряд (например, [♥7] [♥8] [♥9] [♥10]). Такие выкладки условно называем стрит (street).
* Все карты имеют одно значение (например, [♦K] [♠K] [♣K]). Такие выкладки условно называем авеню (avenue).

В выкладку может входить не больше одного джокера. Джокер, как обычно, может заменять любую карту.

В начале игры каждому игроку раздаётся по 3 карты. Ещё одна карта кладётся на стол лицом вверх — она начинает ряд карт, называемый *сбросом* (trash). Остаток колоды кладётся рубашкой вверх.

Ход каждого игрока состоит из следующих стадий:

1. Игрок обязательно берёт одну верхнюю карту из колоды.
2. Игрок может заменить одного из джокеров, входящего в ранее сделанные (им или кем-то другим) выкладки, на заменяемую им карту, если эта карта есть у него в руке. Например, если на столе есть выкладка [♥7] [Black Joker] [♥9], а у игрока есть [♥8], то он может положить восьмёрку в выкладку, а джокера забрать себе. Такая замена разрешена только в том случае, если игрок этим же ходом выложит джокера в составе новой выкладки.
3. Игрок может взять несколько последних карт из сброса. Такое взятие разрешено только в том случае, если первую из взятых карт игрок этим же ходом выложит в составе новой выкладки или добавит в существующую выкладку (см. ниже).
4. Игрок может сделать одну или несколько новых выкладок, если у него в руке есть подходящие карты. Карты, входящие в эти выкладки, считаются принадлежащими тому игроку, который выложил эти карты.
5. Игрок может добавить одну или несколько карт, имеющихся у него в руке, к существующим выкладкам (своим или чужим), если карты подходят по масти или значению. Джокеров добавлять к выкладкам нельзя. Карты, добавленные к выкладкам, считаются принадлежащими тому игроку, который их выложил, независимо от того, кто изначально сделал выкладку. Таким образом, в общем случае в одну выкладку входят карты, принадлежащие разным игрокам. При игре "вживую" обычно каждый игрок кладёт выложенные им карты рядом с собой, чтобы было видно, где чьё. В данной реализации вся выкладка показывается в один ряд и под каждой картой подписывается, кому она принадлежит.
6. Игрок обязан положить одну карту в сброс.

В некоторых ситуациях игрок по описанным выше правилам может сделать такой ход, что у него в руке не останется карт. Такой ход называется *выходом*. Выход разрешён только в том случае, если в одной из выкладок есть как минимум 4 карты, принадлежащие данному игроку.
Когда один из игроков выходит, игра завершается, и производится подсчёт очков.

Результат игрока = сумма карт в выкладках, принадлежащих игроку - сумма карт в руке + бонус.

Карты оцениваются следующим образом:

* От 2 до 10 — соответственно, от 2 до 10 очков.
* Валет 11, дама 12, король 13, туз 14.
* Джокер, входящий в выкладку, считается за ту карту, которую он заменяет.
* Джокер, оставшийся у игрока в руке, стоит 50 очков.

Бонус равен 50 очкам для того игрока, который произвёл выход, и 0 для остальных.

Пример хода
===========

Пусть в какой-то момент на столе лежит такой сброс:
  ♥2 ♦3 ♣Q ♦5 ♦9 ♦6 ♣10 ♥8 ♦10 ♣9
(карты, выложенные последними, показаны слева).
Кроме того, лежит такая выкладка, сделанная на последнем ходу предыдущим игроком:
  ♣4 ♣5 ♣6
Перед ходом у игрока в руке такие карты:
  ♣7 ♦9 ♥Q

* Игрок берёт одну карту из колоды, и это оказывается, например, ♦4.
* Игрок берёт последние 6 карт из сброса. В результате у него в руке ♣7 ♦9 ♥Q ♦4 ♥2 ♦3 ♣Q ♦5 ♦9 ♦6.
* Игрок делает новую выкладку: ♦4 ♦5 ♦6.
* Игрок добавляет ♣7 к уже лежащей на столе выкладке.
* Игрок кладёт ♥Q в сброс.
* В результате у него в руке остаётся ♦9 ♥2 ♦3 ♣Q ♦9.

Пример расчёта результата
=========================

Если после описанного выше хода ходящий следующим игрок выходит, и у нашего игрока нет других выкладок, то его результат будет:
(4 + 5 + 6 + 7) - (9 + 2 +3 + 12 + 9) + 0 = -13.

Краткое руководство пользователя
================================

Собираем:

$ sudo apt-get install haskell-platform
$ cabal install combinat failure ifElse
$ make

Запускаем:

$ ./LocalGame <игроки>

Здесь <игроки> — список игроков через пробел. Каждый игрок может быть "ai" или "human". Например:

$ ./LocalGame ai ai ai ai

Или:

$ ./LocalGame human ai

Если в игре участвует человек, то ходы будут запрашиваться с stdin, отдельными приглашениями. Карта из колоды даётся игроку перед ходом автоматически. Ходы вводятся на мини-языке. Ход описывается несколькими предложениями, через точку с запятой, каждое предложение соответствует возможному действию в рамках хода. Возможные предложения:

* change <джокер> <номер выкладки c джокером>. Заменить джокера в выкладке на соответствующую карту из руки. Например, change BJ 2.
* pick <количество карт>. Взять карты из сброса. Например, pick 3.
* meld <карты>. Сделать новую выкладку. Например, meld 4S 5S 6S.
* add <карта> <номер выкладки>. Добавить карту из руки к существующей выкладке. Например, add 7C 1.
* trash <карта>. Положить карту в сброс. Например, trash 7D.

Соответствие хода правилам будет проверено автоматически. Если не удалось разобрать описание хода или он не соответствует правилам — будет выдано сообщение и ход будет спрошен вновь.

Карты при вводе записываются как <достоинство><масть>. Достоинства 2-10 записываются соответствующим числом, валет - J, дама - Q, король - K, туз - A. Масти: крести C, бубны D, червы H, пики S. Красный и чёрный джокер — соответственно RJ и BJ.

Например, описанный выше ход задаётся строкой:
pick 6; meld 4D 5D 6D; add 7C 0; trash QH