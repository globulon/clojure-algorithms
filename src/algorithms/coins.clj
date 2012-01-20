(ns algorithms.coins)

(defn- value-for [kind-of-coins]
	(cond 
		(= 1 kind-of-coins) 1
		(= 2 kind-of-coins) 2
		(= 3 kind-of-coins) 5
		(= 4 kind-of-coins) 10
		(= 5 kind-of-coins) 20
		(= 6 kind-of-coins) 50
		(= 7 kind-of-coins) 100
		(= 8 kind-of-coins) 200))

(defn find-combinations-for [amount all-kinds]
	(cond 
		(= amount 0) 1
		(or (< amount 0) (= all-kinds 0)) 0
		:else
			(+ 
				(find-combinations-for 
					amount 
					(dec all-kinds))
				(find-combinations-for 
					(- amount (value-for all-kinds)) 
					all-kinds))))

(defn combinations-for [amount]
	(find-combinations-for amount 8))

