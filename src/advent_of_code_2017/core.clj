(ns advent-of-code-2017.core
  (:import (jdk.internal.util.xml.impl Input)))

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

(comment

  (problem-3a input-3)
  ;; 552

  )



