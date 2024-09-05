#lang scheme

(define customer-list
  '(
    ("John Smith" 35 "New York")
    ("Alice Johnson" 28 "Los Angeles")
    ("Michael Brown" 45 "Miami")
    ("Emily Davis" 32 "Houston")
    ("Robert Wilson" 40 "Miami")
    ("Sophia Martinez" 30 "New York")
    ("William Taylor" 38 "Houston")
    ("Emma White" 25 "Los Angeles")
    ("James Harris" 32 "Houston")
    ("Olivia Clark" 29 "Los Angeles")
    ))

(define item-list
  '(
    ("Apples" "Fruits" 2.30)
    ("Coffee" "Beverages" 3.50)
    ("Bread" "Bakery" 2.00)
    ("Milk" "Dairy" 3.50)
    ("Bananas" "Fruits" 1.75)
    ("Eggs" "Dairy" 4.75)
    ("Orange Juice" "Beverages" 3.25)
    ("Tea" "Beverages" 2.75)
    ("Fish" "Seafood" 12.50)
    ("Broccoli" "Vegetables" 1.80)
    ("Orange" "Fruits" 1.25)
    ("Chicken" "Meat" 7.00)
    ("Lettuce" "Vegetables" 1.20)
    ("Pasta" "Pantry" 3.75)
    ("Salmon" "Seafood" 9.50)
    ("Yogurt" "Dairy" 2.75)
    ("Bacon" "Meat" 6.25)
    ("Cheese" "Dairy" 5.50)
    ("Beef" "Meat" 8.00)
    ("Potatoes" "Vegetables" 2.50)
    ("Chicken Soup" "Canned Goods" 3.50)
    ("Rice" "Grains" 2.25)
    ("Carrots" "Vegetables" 1.10)
    ("Spinach" "Vegetables" 1.60)
    ("Tomatoes" "Vegetables" 1.50)
    ("Apple Juice" "Beverages" 3.40)
    ("Onions" "Vegetables" 1.20)
    ))

(define transaction-list
  '(
    ("John Smith" ("Apples" "Coffee" "Bread") "15.03.2024")
    ("John Smith" ("Milk" "Bananas") "22.03.2024")
    ("John Smith" ("Eggs" "Orange Juice") "29.03.2024")
    ("John Smith" ("Tea" "Fish" "Broccoli" "Orange") "5.04.2024")
    ("John Smith" ("Chicken" "Lettuce" "Pasta" "Salmon") "12.04.2024")
    ("Alice Johnson" ("Milk" "Bananas") "20.03.2024")
    ("Michael Brown" ("Orange Juice" "Yogurt") "24.03.2024")
    ("Michael Brown" ("Bacon") "28.03.2024")
    ("Michael Brown" ("Coffee" "Bread" "Apples") "2.04.2024")
    ("Michael Brown" ("Milk" "Bananas" "Eggs") "5.04.2024")
    ("Michael Brown" ("Cheese" "Beef" "Potatoes" "Chicken Soup") "10.04.2024")
    ("Emily Davis" ("Chicken" "Lettuce") "24.03.2024")
    ("Emily Davis" ("Pasta" "Salmon" "Rice" "Potatoes") "28.03.2024")
    ("Emily Davis" ("Carrots" "Spinach") "1.04.2024")
    ("Robert Wilson" ("Salmon" "Rice") "21.03.2024")
    ("Robert Wilson" ("Potatoes" "Chicken" "Lettuce" "Pasta") "25.03.2024")
    ("Robert Wilson" ("Milk" "Bananas" "Eggs" "Orange Juice") "29.03.2024")
    ("Robert Wilson" ("Bacon") "2.04.2024")
    ("Robert Wilson" ("Fish" "Broccoli") "6.04.2024")
    ("Sophia Martinez" ("Carrots" "Spinach") "26.03.2024")
    ("Sophia Martinez" ("Tea" "Fish" "Broccoli" "Orange") "30.03.2024")
    ("William Taylor" ("Beef" "Potatoes") "19.03.2024")
    ("William Taylor" ("Chicken Soup" "Tomatoes" "Apple Juice" "Bread") "23.03.2024")
    ("Emma White" ("Tomatoes" "Chicken Soup") "23.03.2024")
    ("Emma White" ("Milk" "Salmon" "Rice" "Potatoes") "27.03.2024")
    ("Emma White" ("Chicken" "Lettuce" "Pasta" "Salmon") "31.03.2024")
    ("James Harris" ("Onions" "Apple Juice") "25.03.2024")
    ("James Harris" ("Cheese" "Beef" "Potatoes" "Chicken Soup") "29.03.2024")
    ("Olivia Clark" ("Fish" "Broccoli") "25.03.2024")
    ("Olivia Clark" ("Orange" "Chicken" "Lettuce" "Pasta") "29.03.2024")
    ))

(define (displayln . args)
  (for-each display args)
  (newline)
  (newline))

(define (exists pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) #t)
        (else (exists pred (cdr lst)))))

;1

(define (return-unique-items item-list)
  (remove-duplicates (map car item-list)))

(display "1. A list of all unique items: ")
(displayln (return-unique-items item-list))


;2

(define (return-unique-categories item-list)
  (remove-duplicates (map (lambda (elements) (list-ref elements 1)) item-list)))

(display "2. A list of all unique categories: ")
(displayln (return-unique-categories item-list))


;3

(define (items-by-category category)
  (map car(filter (lambda (item) (equal? (cadr item) category)) item-list)))

(display "3. Items belonging to Vegetables: ")
(displayln (items-by-category "Vegetables"))


;4

(define (get-customer-info name)
  (if (assoc name customer-list)
      (assoc name customer-list)
      '("Not found")))

(display "4. Retrieve information about John Smith: ")
(displayln (get-customer-info "John Smith"))


;5

(define (get-item-info name)
  (if (assoc name item-list)
      (assoc name item-list)
      '("Not found")))

(display "5. Retrieve information about Tea: ")
(displayln (get-item-info "Tea"))


;6

(define (return-items-price item-list)
  (map (lambda (lst) (list-ref lst 2)) item-list))

(define (fact-iter max-price counter list-size)
  (if (> counter list-size)
      max-price
      (if (> counter (- list-size 1))                     
          max-price
      (fact-iter (if (> (list-ref (return-items-price item-list) counter) max-price)
                     (list-ref (return-items-price item-list) counter)
                     max-price)                               
                (+ counter 1)
                list-size)
      ))
  )

(define (return-most-expensive-item)
  (let ((max_price (fact-iter 0 0 (length item-list))))
    (remove-duplicates (filter (lambda (lst) (= (list-ref lst 2) max_price)) item-list))))

(display "6. The most expensive item: ")
(displayln (return-most-expensive-item))


;7

(define (fact-iterr min-price counter list-size)
  (if (> counter list-size)
      min-price
      (if (> counter (- list-size 1))                     
          min-price
      (fact-iterr (if (< (list-ref (return-items-price item-list) counter) min-price)
                     (list-ref (return-items-price item-list) counter)
                     min-price)                               
                (+ counter 1)
                list-size)
      ))
  )

(define (return-cheapest-item)
  (let ((min_price (fact-iterr 999 0 (length item-list))))
    (remove-duplicates (filter (lambda (lst) (= (list-ref lst 2) min_price)) item-list))))

(display "7. The cheapest item: ")
(displayln (return-cheapest-item))


;8

(define (get-transaction-items customer-name)
    (filter (lambda (transaction) (equal? (car transaction) customer-name)) transaction-list))

(define (get-item-cost item-name)
  (let ((item (assoc item-name item-list)))
    (if item
        (caddr item)
        0)))

(define (calculate-transaction-cost transaction)
    (apply + (map get-item-cost (list-ref transaction 1))))

(define (calculate-total-cost transactions)
    (apply + (map calculate-transaction-cost transactions)))

(display "8. Total cost of all items purchased for Olivia Clark: ")
(displayln (calculate-total-cost (get-transaction-items "Olivia Clark")))


;9

(define (append-transaction-items transactions)
  (apply append (map cadr transactions)))

(define (retrieve-all-items name)
  (append-transaction-items(get-transaction-items name)))

(display "9. Items bought by Olivia Clark: ")
(displayln (retrieve-all-items "Olivia Clark"))


;10

(define (calculate-total-cost-of-all-transactions)
  (calculate-total-cost transaction-list))

(display "10. Total cost of all transactions: ")
(displayln (calculate-total-cost-of-all-transactions))


;11

(define (items-purchased-by-date date)
  (define (filter-by-date transaction)
    (equal? (caddr transaction) date))
  
  (define (extract-items transaction)
    (cadr transaction))
  
  (apply append (map extract-items (filter filter-by-date transaction-list))))

(display "11. Items purchased by date 31.03.2024: ")
(displayln (items-purchased-by-date "31.03.2024"))


;12

(define (total-revenue-by-category category)
  (define (filter-by-category transaction)
    (let ((items (cadr transaction)))
      (exists (lambda (item)
                (let ((item-info (get-item-info item)))
                  (and item-info
                       (equal? (cadr item-info) category))))
              items)))
  
  (define (calculate-transaction-cost transaction)
    (apply +
           (map (lambda (item)
                  (let ((item-info (get-item-info item)))
                    (if (and item-info
                             (equal? (cadr item-info) category))
                        (caddr item-info)
                        0)))
                (cadr transaction))))
  
  (define (sum-revenue transactions)
    (apply + (map calculate-transaction-cost transactions)))
  
  (sum-revenue (filter filter-by-category transaction-list)))

(define (total-revenue-for-all-categories)
  (define unique-categories (return-unique-categories item-list))
  
  (map (lambda (category)
         (list category (total-revenue-by-category category)))
       unique-categories))

(display "12. Total revenue generated by each category: ")
(displayln (total-revenue-for-all-categories))


;13

(define (get-category-occurrences category)
  (define (filter-by-category transaction)
    (let ((items (cadr transaction)))
      (exists (lambda (item)
                (let ((item-info (get-item-info item)))
                  (and item-info
                       (equal? (cadr item-info) category))))
              items)))
  
  (define (get-transactions-with-category transactions)
    (length (filter filter-by-category transactions)))
  
  (get-transactions-with-category transaction-list))

(define (most-popular-category)
  (define unique-categories (return-unique-categories item-list))
  
  (define (get-category-occurrences-for-all-categories categories)
    (map (lambda (category)
           (list category (get-category-occurrences category)))
         categories))
  
  (define category-occurrences (get-category-occurrences-for-all-categories unique-categories))
  
  (define (find-max-occurrences categories)
    (let loop ((max-category (car categories))
               (max-occurrences (cadr (car categories)))
               (rest-categories (cdr categories)))
      (if (null? rest-categories)
          max-category
          (let ((current-category (car (car rest-categories)))
                (current-occurrences (cadr (car rest-categories))))
            (if (> current-occurrences max-occurrences)
                (loop current-category current-occurrences (cdr rest-categories))
                (loop max-category max-occurrences (cdr rest-categories)))))))
  
  (find-max-occurrences category-occurrences))

(display "13. Most popular category based on the number of items sold: ")
(displayln (most-popular-category))


;14

(define (all-items-purchased-by-age min-age max-age)
  (define (filter-by-age-range transaction)
    (let* ((customer-name (car transaction))
           (customer-info (get-customer-info customer-name))
           (customer-age (cadr customer-info)))
      (and (> customer-age min-age) (< customer-age max-age))))
  
  (define (extract-items transaction)
    (cadr transaction))
  
  (define (remove-duplicates lst)
    (cond ((null? lst) '())
          ((member (car lst) (cdr lst))
           (remove-duplicates (cdr lst)))
          (else
           (cons (car lst)
                 (remove-duplicates (cdr lst))))))
  
  (let ((transactions (filter filter-by-age-range transaction-list)))
    (remove-duplicates (apply append (map extract-items transactions)))))

(display "14. Items purchased by customers aged between 20 and 30: ")
(displayln (all-items-purchased-by-age 20 30))


;15

(define (average-spending-per-location transaction-list)
  (define (average-spending location-transactions)
    (let ((total (apply + location-transactions)))
      (if (zero? (length location-transactions))
          0
          (/ total (length location-transactions)))))

  (map (lambda (location)
         (cons location (average-spending (map calculate-transaction-cost
              (filter (lambda (transaction) (equal? location (caddr (get-customer-info (car transaction))))) transaction-list)))))
       (remove-duplicates (map (lambda (customer) (caddr (get-customer-info (car customer)))) transaction-list))))

(display "15. Average spending per transaction by location: ")
(displayln (average-spending-per-location transaction-list))