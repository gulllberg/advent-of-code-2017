(ns advent-of-code-2017.core
  (:require [ysera.test :refer [is= is is-not]]
            [ysera.error :refer [error]]
            [ysera.collections :refer [seq-contains?]]))

;; Day 1

;You're standing in a room with "digitization quarantine" written in LEDs along one wall. The only door is locked, but it includes a small interface. "Restricted Area - Strictly No Digitized Users Allowed."
;
;It goes on to explain that you may only leave by solving a captcha to prove you're not a human. Apparently, you only get one millisecond to solve the captcha: too fast for a normal human, but it feels like hours to you.
;
;The captcha requires you to review a sequence of digits (your puzzle input) and find the sum of all digits that match the next digit in the list. The list is circular, so the digit after the last digit is the first digit in the list.
;
;For example:
;
;1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the third digit (2) matches the fourth digit.
;1111 produces 4 because each digit (all 1) matches the next.
;1234 produces 0 because no digit matches the next.
;91212129 produces 9 because the only digit that matches the next one is the last digit, 9.

(def input-1 "5255443714755555317777152441826784321918285999594221531636242944998363716119294845838579943562543247239969555791772392681567883449837982119239536325341263524415397123824358467891963762948723327774545715851542429832119179139914471523515332247317441719184556891362179267368325486642376685657759623876854958721636574219871249645773738597751429959437466876166273755524873351452951411628479352522367714269718514838933283861425982562854845471512652555633922878128558926123935941858532446378815929573452775348599693982834699757734714187831337546474515678577158721751921562145591166634279699299418269158557557996583881642468274618196335267342897498486869925262896125146867124596587989531495891646681528259624674792728146526849711139146268799436334618974547539561587581268886449291817335232859391493839167111246376493191985145848531829344198536568987996894226585837348372958959535969651573516542581144462536574953764413723147957237298324458181291167587791714172674717898567269547766636143732438694473231473258452166457194797819423528139157452148236943283374193561963393846385622218535952591588353565319432285579711881559343544515461962846879685879431767963975654347569385354482226341261768547328749947163864645168428953445396361398873536434931823635522467754782422557998262858297563862492652464526366171218276176258582444923497181776129436396397333976215976731542182878979389362297155819461685361676414725597335759976285597713332688275241271664658286868697167515329811831234324698345159949135474463624749624626518247831448143876183133814263977611564339865466321244399177464822649611969896344874381978986453566979762911155931362394192663943526834148596342268321563885255765614418141828934971927998994739769141789185165461976425151855846739959338649499379657223196885539386154935586794548365861759354865453211721551776997576289811595654171672259129335243531518228282393326395241242185795828261319215164262237957743232558971289145639852148197184265766291885259847236646615935963759631145338159257538114359781854685695429348428884248972177278361353814766653996675994784195827214295462389532422825696456457332417366426619555")

;; 1. Gå igenom strängen
;; 2. Jämför varje siffra med nästa (sista med första)
;; 3. Om dom är lika, summera siffran till totalen

(defn problem-1a
  [input]
  (reduce (fn [sum index]
            (let [number (-> (subs input index (inc index))
                             (read-string))
                  next-number (-> (if (= (inc index) (count input))
                                    (subs input 0 1)
                                    (subs input (inc index) (+ 2 index)))
                                  (read-string))]
              (if (= number next-number)
                (+ sum number)
                sum)))
          0
          (range (count input))))

;Now, instead of considering the next digit, it wants you to consider the digit halfway around the circular list. That is, if your list contains 10 items, only include a digit in your sum if the digit 10/2 = 5 steps forward matches it. Fortunately, your list has an even number of elements.
;
;For example:
;
;1212 produces 6: the list contains 4 items, and all four digits match the digit 2 items ahead.
;1221 produces 0, because every comparison is between a 1 and a 2.
;123425 produces 4, because both 2s match each other, but no other digit has a match.
;123123 produces 12.
;12131415 produces 4.

;; Om någon matchar i första halvan kommer den matcha med samma i andra halvan
;; --> Gå bara igenom halva strängen

(defn problem-1b
  [input]
  (reduce (fn [sum index]
            (let [number (-> (subs input index (inc index))
                             (read-string))
                  index-plus-half (+ index (/ (count input) 2))
                  comparing-number (-> (subs input index-plus-half (inc index-plus-half))
                                       (read-string))]
              (if (= number comparing-number)
                (+ sum (* 2 number))
                sum)))
          0
          (range (/ (count input) 2))))

(comment

  (problem-1a input-1)
  ;; 1049

  (problem-1b input-1)
  ;; 1508

  )

;; Day 2

;As you walk through the door, a glowing humanoid shape yells in your direction. "You there! Your state appears to be idle. Come help us repair the corruption in this spreadsheet - if we take another millisecond, we'll have to display an hourglass cursor!"
;
;The spreadsheet consists of rows of apparently-random numbers. To make sure the recovery process is on the right track, they need you to calculate the spreadsheet's checksum. For each row, determine the difference between the largest value and the smallest value; the checksum is the sum of all of these differences.
;
;For example, given the following spreadsheet:
;
;5 1 9 5
;7 5 3
;2 4 6 8
;The first row's largest and smallest values are 9 and 1, and their difference is 8.
;The second row's largest and smallest values are 7 and 3, and their difference is 4.
;The third row's difference is 6.
;In this example, the spreadsheet's checksum would be 8 + 4 + 6 = 18.
;
;What is the checksum for the spreadsheet in your puzzle input?

(def input-2 (slurp "assets/input-2.txt"))

(defn problem-2a
  [input]
  (reduce (fn [sum row-string]
            (let [row-numbers (->> (clojure.string/split row-string #"\t")
                                   (map read-string))]
              (+ sum (- (apply max row-numbers) (apply min row-numbers)))))
          0
          (clojure.string/split-lines input)))

;"Based on what we're seeing, it looks like all the User wanted is some information about the evenly divisible values in the spreadsheet. Unfortunately, none of us are equipped for that kind of calculation - most of us specialize in bitwise operations."
;
;It sounds like the goal is to find the only two numbers in each row where one evenly divides the other - that is, where the result of the division operation is a whole number. They would like you to find those numbers on each line, divide them, and add up each line's result.
;
;For example, given the following spreadsheet:
;
;5 9 2 8
;9 4 7 3
;3 8 6 5
;In the first row, the only two numbers that evenly divide are 8 and 2; the result of this division is 4.
;In the second row, the two numbers are 9 and 3; the result is 3.
;In the third row, the result is 2.
;In this example, the sum of the results would be 4 + 3 + 2 = 9.
;
;What is the sum of each row's result in your puzzle input?

(defn problem-2b
  [input]
  (reduce (fn [sum row-string]
            (let [row-numbers (->> (clojure.string/split row-string #"\t")
                                   (map read-string))]
              (+ sum (reduce (fn [result index]
                               (let [number (nth row-numbers index)
                                     compare-numbers (drop (inc index) row-numbers)
                                     matching-number (first (filter (fn [compare-number]
                                                                      (or (= 0 (mod number compare-number))
                                                                          (= 0 (mod compare-number number))))
                                                                    compare-numbers))]
                                 (if (nil? matching-number)
                                   result
                                   (if (= 0 (mod number matching-number))
                                     (/ number matching-number)
                                     (/ matching-number number)))))
                             0
                             (range (count row-numbers))))))
          0
          (clojure.string/split-lines input)))

(defn problem-2b'
  [input]
  (reduce (fn [sum row-string]
            (let [row-numbers (->> (clojure.string/split row-string #"\t")
                                   (map read-string))]
              (+ sum (loop [number-index 0
                            compare-index 1]
                       (let [number (nth row-numbers number-index)
                             compare-number (nth row-numbers compare-index)]
                         (cond
                           ;; Don't compare to itself
                           (= number-index compare-index)
                           (recur number-index (inc compare-index))

                           (= 0 (mod number compare-number))
                           (/ number compare-number)

                           (= 0 (mod compare-number number))
                           (/ compare-number number)

                           ;; Compared number to all other numbers
                           (= compare-index (dec (count row-numbers)))
                           (recur (inc number-index) 0)

                           :else
                           (recur number-index (inc compare-index))
                           ))))))
          0
          (clojure.string/split-lines input)))

(comment

  (problem-2a input-2)
  ;; 21845

  (problem-2b input-2)
  ;; 191
  (problem-2b' input-2)
  )

;; Day 3

(def input-3 325489)

;You come across an experimental new kind of memory stored on an infinite two-dimensional grid.
;
;Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while spiraling outward. For example, the first few squares are allocated like this:
;
;17  16  15  14  13
;18   5   4   3  12
;19   6   1   2  11
;20   7   8   9  10
;21  22  23---> ...
;While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the location of the only access port for this memory system) by programs that can only move up, down, left, or right. They always take the shortest path: the Manhattan Distance between the location of the data and square 1.
;
;For example:
;
;Data from square 1 is carried 0 steps, since it's at the access port.
;Data from square 12 is carried 3 steps, such as: down, left, left.
;Data from square 23 is carried only 2 steps: up twice.
;Data from square 1024 must be carried 31 steps.
;How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?

(defn get-level
  [input]
  (some (fn [level]
          (let [side (- (* 2 level) 1)]
            (when (>= (* side side) input)
              level)))
        (range)))

(defn get-side-steps
  [input]
  (let [level (get-level input)
        level-length (- (* 2 level) 1)
        previous-level-length (- (* 2 (dec level)) 1)
        step-number (- input (* previous-level-length previous-level-length))
        mod-step-number (mod step-number (dec level-length))
        max-side-distance (/ (dec level-length) 2)]
    (Math/abs (- max-side-distance mod-step-number))
    ))

(defn problem-3a
  [input]
  (+ (dec (get-level input))
     (get-side-steps input)))

;As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1. Then, in the same allocation order as shown above, they store the sum of the values in all adjacent squares, including diagonals.
;
;So, the first few squares' values are chosen as follows:
;
;Square 1 starts with the value 1.
;Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
;Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
;Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
;Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
;Once a square is written, its value does not change. Therefore, the first few squares would receive the following values:
;
;147  142  133  122   59
;304    5    4    2   57
;330   10    1    1   54
;351   11   23   25   26
;362  747  806--->   ...
;What is the first value written that is larger than your puzzle input?

(defn problem-3b
  [input]
  (loop [[x y] [0 1]
         grid {"0+0" 1}]
    (let [north (get grid (str x "+" (inc y)))
          north-west (get grid (str (dec x) "+" (inc y)))
          west (get grid (str (dec x) "+" y))
          south-west (get grid (str (dec x) "+" (dec y)))
          south (get grid (str x "+" (dec y)))
          south-east (get grid (str (inc x) "+" (dec y)))
          east (get grid (str (inc x) "+" y))
          north-east (get grid (str (inc x) "+" (inc y)))
          sum (->> [north north-west west south-west south south-east east north-east]
                   (map (fn [v] (or v 0)))
                   (apply +))
          next-direction (cond
                           (and (not (nil? north))
                                (not (nil? west)))
                           [1 0]

                           (not (nil? west))
                           [0 1]

                           (not (nil? south))
                           [-1 0]

                           (not (nil? east))
                           [0 -1]

                           (not (nil? north))
                           [1 0]

                           :else
                           [1 0])]
      (if (> sum input)
        sum
        (recur (mapv + [x y] next-direction)
               (assoc grid
                 (str x "+" y) sum
                 (str x "+" (inc y)) north
                 (str (dec x) "+" (inc y)) north-west
                 (str (dec x) "+" y) west
                 (str (dec x) "+" (dec y)) south-west
                 (str x "+" (dec y)) south
                 (str (inc x) "+" (dec y)) south-east
                 (str (inc x) "+" y) east
                 (str (inc x) "+" (inc y)) north-east))))))
(comment

  (problem-3a input-3)
  ;; 552
  (problem-3b input-3)
  ;; 330785
  )


;; Day 4

(def input-4 (slurp "assets/input-4.txt"))

;A new system policy has been put in place that requires all accounts to use a passphrase instead of simply a password. A passphrase consists of a series of words (lowercase letters) separated by spaces.
;
;To ensure security, a valid passphrase must contain no duplicate words.
;
;For example:
;
;aa bb cc dd ee is valid.
;aa bb cc dd aa is not valid - the word aa appears more than once.
;aa bb cc dd aaa is valid - aa and aaa count as different words.
;The system's full passphrase list is available as your puzzle input. How many passphrases are valid?

(defn valid-passphrase-a?
  [passphrase]
  (let [passphrase-list (clojure.string/split passphrase #" ")]
    (= (count passphrase-list)
       (->> passphrase-list
            (set)
            (count)))))

(defn problem-4a
  {:test (fn []
           (is= (problem-4a input-4)
                386))}
  [input]
  (->> (clojure.string/split input #"\n")
       (filter valid-passphrase-a?)
       (count)))

;For added security, yet another system policy has been put in place. Now, a valid passphrase must contain no two words that are anagrams of each other - that is, a passphrase is invalid if any word's letters can be rearranged to form any other word in the passphrase.
;
;For example:
;
;abcde fghij is a valid passphrase.
;abcde xyz ecdab is not valid - the letters from the third word can be rearranged to form the first word.
;a ab abc abd abf abj is a valid passphrase, because all letters need to be used when forming another word.
;iiii oiii ooii oooi oooo is valid.
;oiii ioii iioi iiio is not valid - any of these words can be rearranged to form any other word.
;Under this new system policy, how many passphrases are valid?

(defn valid-passphrase-b?
  {:test (fn []
           (clojure.test/is (not (valid-passphrase-b? "ab ba")))
           (clojure.test/is (valid-passphrase-b? "ab ac")))}
  [passphrase]
  (let [passphrase-list (clojure.string/split passphrase #" ")]
    (= (count passphrase-list)
       (->> passphrase-list
            (map sort)
            (set)
            (count)))))

(defn problem-4b
  {:test (fn []
           (is= (problem-4b input-4)
                208))}
  [input]
  (->> (clojure.string/split input #"\n")
       (filter valid-passphrase-b?)
       (count)))

(clojure.test/deftest day-4
  (clojure.test/is (= (problem-4a input-4) 386))
  (clojure.test/is (= (problem-4b input-4) 208)))

(comment
  (valid-passphrase-a? "aa bb bb")
  (problem-4a input-4)
  ;; 386
  (problem-4b input-4)
  ;; 208
  )

;; Day 5

;The message includes a list of the offsets for each jump. Jumps are relative: -1 moves to the previous instruction, and 2 skips the next one. Start at the first instruction in the list. The goal is to follow the jumps until one leads outside the list.
;
;In addition, these instructions are a little strange; after each jump, the offset of that instruction increases by 1. So, if you come across an offset of 3, you would move three instructions forward, but change it to a 4 for the next time it is encountered.
;
;For example, consider the following list of jump offsets:
;
;0
;3
;0
;1
;-3
;Positive jumps ("forward") move downward; negative jumps move upward. For legibility in this example, these offset values will be written all on one line, with the current instruction marked in parentheses. The following steps would be taken before an exit is found:
;
;(0) 3  0  1  -3  - before we have taken any steps.
;(1) 3  0  1  -3  - jump with offset 0 (that is, don't jump at all). Fortunately, the instruction is then incremented to 1.
;2 (3) 0  1  -3  - step forward because of the instruction we just modified. The first instruction is incremented again, now to 2.
;2  4  0  1 (-3) - jump all the way to the end; leave a 4 behind.
;2 (4) 0  1  -2  - go back to where we just were; increment -3 to -2.
;2  5  0  1  -2  - jump 4 steps forward, escaping the maze.
;In this example, the exit is reached in 5 steps.
;
;How many steps does it take to reach the exit?

(def input-5 (slurp "assets/input-5.txt"))

(defn problem-5a
  [input]
  (loop [list (mapv read-string (clojure.string/split input #"\n"))
         index 0
         number-of-jumps 0]
    (if (or (> 0 index)
            (<= (count list) index))
      number-of-jumps
      (recur (update list index inc)
             (+ index (get list index))
             (inc number-of-jumps)))))

;Now, the jumps are even stranger: after each jump, if the offset was three or more, instead decrease it by 1. Otherwise, increase it by 1 as before.
;
;Using this rule with the above example, the process now takes 10 steps, and the offset values after finding the exit are left as 2 3 2 3 -1.
;
;How many steps does it now take to reach the exit?

(defn problem-5b
  [input]
  (loop [list (mapv read-string (clojure.string/split input #"\n"))
         index 0
         number-of-jumps 0]
    (if (or (> 0 index)
            (<= (count list) index))
      number-of-jumps
      (recur (update list index (fn [offset]
                                  (if (<= 3 offset)
                                    (dec offset)
                                    (inc offset))))
             (+ index (get list index))
             (inc number-of-jumps)))))

(comment
  (problem-5a input-5)
  ;; 391540
  (problem-5b input-5)
  ;; 30513679
  )

;; Day 6

;In this area, there are sixteen memory banks; each memory bank can hold any number of blocks. The goal of the reallocation routine is to balance the blocks between the memory banks.
;
;The reallocation routine operates in cycles. In each cycle, it finds the memory bank with the most blocks (ties won by the lowest-numbered memory bank) and redistributes those blocks among the banks. To do this, it removes all of the blocks from the selected bank, then moves to the next (by index) memory bank and inserts one of the blocks. It continues doing this until it runs out of blocks; if it reaches the last memory bank, it wraps around to the first one.
;
;The debugger would like to know how many redistributions can be done before a blocks-in-banks configuration is produced that has been seen before.
;
;For example, imagine a scenario with only four memory banks:
;
;The banks start with 0, 2, 7, and 0 blocks. The third bank has the most blocks, so it is chosen for redistribution.
;Starting with the next bank (the fourth bank) and then continuing to the first bank, the second bank, and so on, the 7 blocks are spread out over the memory banks. The fourth, first, and second banks get two blocks each, and the third bank gets one back. The final result looks like this: 2 4 1 2.
;Next, the second bank is chosen because it contains the most blocks (four). Because there are four memory banks, each gets one block. The result is: 3 1 2 3.
;Now, there is a tie between the first and fourth memory banks, both of which have three blocks. The first bank wins the tie, and its three blocks are distributed evenly over the other three banks, leaving it with none: 0 2 3 4.
;The fourth bank is chosen, and its four blocks are distributed such that each of the four banks receives one: 1 3 4 1.
;The third bank is chosen, and the same thing happens: 2 4 1 2.
;At this point, we've reached a state we've seen before: 2 4 1 2 was already seen. The infinite loop is detected after the fifth block redistribution cycle, and so the answer in this example is 5.
;
;Given the initial block counts in your puzzle input, how many redistribution cycles must be completed before a configuration is produced that has been seen before?

(def input-6 "11\t11\t13\t7\t0\t15\t5\t5\t4\t4\t1\t1\t7\t1\t15\t11")

;; Day 7

;One program at the bottom supports the entire tower. It's holding a large disc, and on the disc are balanced several more sub-towers. At the bottom of these sub-towers, standing on the bottom disc, are other programs, each holding their own disc, and so on. At the very tops of these sub-sub-sub-...-towers, many programs stand simply keeping the disc below them balanced but with no disc of their own.
;
;You offer to help, but first you need to understand the structure of these towers. You ask each program to yell out their name, their weight, and (if they're holding a disc) the names of the programs immediately above them balancing on that disc. You write this information down (your puzzle input). Unfortunately, in their panic, they don't do this in an orderly fashion; by the time you're done, you're not sure which program gave which information.
;
;For example, if your list is the following:
;
;pbga (66)
;xhth (57)
;ebii (61)
;havc (66)
;ktlj (57)
;fwft (72) -> ktlj, cntj, xhth
;qoyq (66)
;padx (45) -> pbga, havc, qoyq
;tknk (41) -> ugml, padx, fwft
;jptl (61)
;ugml (68) -> gyxo, ebii, jptl
;gyxo (61)
;cntj (57)
;...then you would be able to recreate the structure of the towers that looks like this:
;
;gyxo
;/
;ugml - ebii
;/      \
;|         jptl
;|
;|         pbga
;/        /
;tknk --- padx - havc
;\        \
;|         qoyq
;|
;|         ktlj
;\      /
;fwft - cntj
;\
;xhth
;In this example, tknk is at the bottom of the tower (the bottom program), and is holding up ugml, padx, and fwft. Those programs are, in turn, holding up other programs; in this example, none of those programs are holding up any other programs, and are all the tops of their own towers. (The actual tower balancing in front of you is much larger.)
;
;Before you're ready to help them, you need to make sure your information is correct. What is the name of the bottom program?

(def input-7 (slurp "assets/input-7.txt"))

(defn problem-7a
  [input]
  (let [programs (clojure.string/split input #"\n")
        dependencies (reduce (fn [dependencies program]
                               (let [program-dependencies (clojure.string/split program #"->")
                                     program-dependencies-list (if (= (count program-dependencies) 1)
                                                                 []
                                                                 (-> (nth program-dependencies 1)
                                                                     (clojure.string/trim)
                                                                     (clojure.string/split #", ")))]
                                 (clojure.set/union dependencies (into #{} program-dependencies-list))))
                             #{}
                             programs)
        names (map (fn [program]
                     (-> (clojure.string/split program #" ")
                         (nth 0)))
                   programs)]
    (-> (remove (fn [name] (seq-contains? dependencies name)) names)
        (first)))
  )

;The programs explain the situation: they can't get down. Rather, they could get down, if they weren't expending all of their energy trying to keep the tower balanced. Apparently, one program has the wrong weight, and until it's fixed, they're stuck here.
;
;For any program holding a disc, each program standing on that disc forms a sub-tower. Each of those sub-towers are supposed to be the same weight, or the disc itself isn't balanced. The weight of a tower is the sum of the weights of the programs in that tower.
;
;In the example above, this means that for ugml's disc to be balanced, gyxo, ebii, and jptl must all have the same weight, and they do: 61.
;
;However, for tknk to be balanced, each of the programs standing on its disc and all programs above it must each match. This means that the following sums must all be the same:
;
;ugml + (gyxo + ebii + jptl) = 68 + (61 + 61 + 61) = 251
;padx + (pbga + havc + qoyq) = 45 + (66 + 66 + 66) = 243
;fwft + (ktlj + cntj + xhth) = 72 + (57 + 57 + 57) = 243
;As you can see, tknk's disc is unbalanced: ugml's stack is heavier than the other two. Even though the nodes above ugml are balanced, ugml itself is too heavy: it needs to be 8 units lighter for its stack to weigh 243 and keep the towers balanced. If this change were made, its weight would be 60.
;
;Given that exactly one program is the wrong weight, what would its weight need to be to balance the entire tower?

(defn get-dependencies
  [programs program]
  (let [program-dependencies (clojure.string/split program #"->")
        program-dependencies-vector (if (= (count program-dependencies) 1)
                                      []
                                      (-> (nth program-dependencies 1)
                                          (clojure.string/trim)
                                          (clojure.string/split #", ")))]
    (filter (fn [program]
              (seq-contains? program-dependencies-vector (-> (clojure.string/split program #" ")
                                                             (nth 0))))
            programs)))

(defn get-weight
  [program]
  (-> (clojure.string/split program #" ")
      (second)
      (read-string)
      (nth 0)))

(defn get-total-weight
  [programs program]
  (apply +
         (get-weight program)
         (map (fn [dependency]
                (get-total-weight programs dependency))
              (get-dependencies programs program))))

(defn problem-7b
  [input]
  (let [programs (clojure.string/split input #"\n")
        unbalanced-program (->> programs
                                (filter (fn [program]
                                          (let [dependencies (get-dependencies programs program)]
                                            (and (> (count dependencies) 0)
                                                 (apply not= (map (fn [dependency]
                                                                    (get-total-weight programs dependency))
                                                                  dependencies))))))
                                (sort-by (fn [program]
                                           (get-total-weight programs program)))
                                (first))
        unbalanced-program-dependencies (get-dependencies programs unbalanced-program)
        sorted-unbalanced-program-dependencies (sort-by (fn [dependency]
                                                          (get-total-weight programs dependency))
                                                        unbalanced-program-dependencies)
        sorted-unbalanced-program-dependencies-weights (map (fn [dependency]
                                                              (get-total-weight programs dependency))
                                                            sorted-unbalanced-program-dependencies)]
    (if (= (first sorted-unbalanced-program-dependencies-weights)
           (second sorted-unbalanced-program-dependencies-weights))
      (- (get-weight (last sorted-unbalanced-program-dependencies))
         (- (apply max sorted-unbalanced-program-dependencies-weights)
            (apply min sorted-unbalanced-program-dependencies-weights)))
      (+ (get-weight (first sorted-unbalanced-program-dependencies))
         (- (apply max sorted-unbalanced-program-dependencies-weights)
            (apply min sorted-unbalanced-program-dependencies-weights))))))

(comment
  (problem-7a input-7)
  ;; hlqnsbe

  (problem-7b input-7)
  ;; 1993
  )

;; Day 8

(def input-8 (slurp "assets/input-8.txt"))

;You receive a signal directly from the CPU. Because of your recent assistance with jump instructions, it would like you to compute the result of a series of unusual register instructions.
;
;Each instruction consists of several parts: the register to modify, whether to increase or decrease that register's value, the amount by which to increase or decrease it, and a condition. If the condition fails, skip the instruction without modifying the register. The registers all start at 0. The instructions look like this:
;
;b inc 5 if a > 1
;a inc 1 if b < 5
;c dec -10 if a >= 1
;c inc -20 if c == 10
;These instructions would be processed as follows:
;
;Because a starts at 0, it is not greater than 1, and so b is not modified.
;a is increased by 1 (to 1) because b is less than 5 (it is 0).
;c is decreased by -10 (to 10) because a is now greater than or equal to 1 (it is 1).
;c is increased by -20 (to -10) because c is equal to 10.
;After this process, the largest value in any register is 1.
;
;You might also encounter <= (less than or equal to) or != (not equal to). However, the CPU doesn't have the bandwidth to tell you what all the registers are named, and leaves that to you to determine.
;
;What is the largest value in any register after completing the instructions in your puzzle input?

(defn problem-8a
  {:test (fn []
           (is= (problem-8a "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10\n")
                1))}
  [input]
  (let [instructions (-> (clojure.string/replace input #"!=" "not=")
                         (clojure.string/replace #"==" "=")
                         (clojure.string/replace #"inc" "+")
                         (clojure.string/replace #"dec" "-")
                         (clojure.string/split #"\n"))]
    (->> instructions
         (reduce (fn [registers instruction]
                  (let [instruction-parts (clojure.string/split instruction #" ")
                        register-to-modify (nth instruction-parts 0)
                        modification-fn (resolve (symbol (nth instruction-parts 1)))
                        modification-amount (read-string (nth instruction-parts 2))
                        condition-register (nth instruction-parts 4)
                        condition-operator (resolve (symbol (nth instruction-parts 5)))
                        condition-amount (read-string (nth instruction-parts 6))]
                    (as-> registers $
                          (if (contains? $ register-to-modify)
                            $
                            (assoc $ register-to-modify 0))
                          (if (contains? $ condition-register)
                            $
                            (assoc $ condition-register 0))
                          (if (condition-operator (get $ condition-register) condition-amount)
                            (update $ register-to-modify modification-fn modification-amount)
                            $))))
                {})
        (vals)
        (apply max))))

;To be safe, the CPU also needs to know the highest value held in any register during this process so that it can decide how much memory to allocate to these operations. For example, in the above instructions, the highest value ever held was 10 (in register c after the third instruction was evaluated).

(defn problem-8b
  {:test (fn []
           (is= (problem-8b "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10\n")
                10))}
  [input]
  (let [instructions (-> (clojure.string/replace input #"!=" "not=")
                         (clojure.string/replace #"==" "=")
                         (clojure.string/replace #"inc" "+")
                         (clojure.string/replace #"dec" "-")
                         (clojure.string/split #"\n"))]
    (->> instructions
      (reduce (fn [[registers max-value] instruction]
                   (let [instruction-parts (clojure.string/split instruction #" ")
                         register-to-modify (nth instruction-parts 0)
                         modification-fn (resolve (symbol (nth instruction-parts 1)))
                         modification-amount (read-string (nth instruction-parts 2))
                         condition-register (nth instruction-parts 4)
                         condition-operator (resolve (symbol (nth instruction-parts 5)))
                         condition-amount (read-string (nth instruction-parts 6))]
                     (as-> registers $
                           (if (contains? $ register-to-modify)
                             $
                             (assoc $ register-to-modify 0))
                           (if (contains? $ condition-register)
                             $
                             (assoc $ condition-register 0))
                           (if (condition-operator (get $ condition-register) condition-amount)
                             (update $ register-to-modify modification-fn modification-amount)
                             $)
                           [$ (if (> (apply max (vals $)) max-value)
                                (apply max (vals $))
                                max-value)])))
                [{} 0])
         (second))))

(comment
  (problem-8a input-8)
  ; 5143

  (problem-8b input-8)
  ; 6209
  )

