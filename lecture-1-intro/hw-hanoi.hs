type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n from to spare = hanoi (n - 1) from spare to ++
                        [(from, to)] ++
                        hanoi (n - 1) spare to from

hanoi2 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi2 0 _ _ _ _ = []
hanoi2 1 from to _ _ = [(from, to)]
hanoi2 n from to spare1 spare2 = hanoi2 (n - 2) from spare1 spare2 to ++
                                 [(from, spare2), (from, to), (spare2, to)] ++
                                 hanoi2 (n - 2) spare1 to from spare2
