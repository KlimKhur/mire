(ns mire.player
  (:require [mire.rooms :as rooms])
  (:refer-clojure :exclude [alter])
  (:require [clojure.core :refer :all]))

(def ^:dynamic *name* nil)
(def ^:dynamic *current-room* nil)
(def ^:dynamic *inventory* (ref #{}))
(def ^:dynamic *equipment* (ref #{}))
(def ^:dynamic *health* (ref 5))
(def ^:dynamic *mana* (ref 5))
(def ^:dynamic *spells* (ref #{}))
(def ^:dynamic *visited-rooms* (ref 0))
(def ^:dynamic *invisible-turns* (ref 0))
(def ^:dynamic *resurrect* (ref false))
(def ^:dynamic *poisoned* (ref false))

(def prompt "> ")
(def streams (ref {}))

(defn carrying? [thing]
  (some #{(keyword thing)} @*inventory*))

(defn equipped? [thing]
  (some #{(keyword thing)} @*equipment*))

(defn inventory-full? []
  (>= (count @*inventory*) 10))

(defn equipment-full? []
  (>= (count @*equipment*) 6))

(defn take-damage [damage]
  (let [reduced-damage (if (equipped? :shield) (max 0 (- damage 1)) damage)
        final-damage (if (equipped? :enchanted_armor) (max 0 (- reduced-damage 2)) reduced-damage)]
    (dosync
      (alter *health* - final-damage)
      (if (and (<= @*health* 0) @*resurrect*)
        (do
          (alter *resurrect* (constantly false))
          (alter *health* (constantly 5))
          "Вы воскресли благодаря Перу Феникса!")
        final-damage))))

(defn attack [monster]
  (let [weapon-damage (cond
                        (equipped? :magic_sword) 3
                        (equipped? :blazing_sword) 3
                        (equipped? :thunder_hammer) 2
                        (equipped? :dragon_bow) 4
                        (equipped? :shadow_dagger) 2
                        (equipped? :titan_axe) 3
                        :else 1)]
    weapon-damage))

(defn learn-spell [spell]
  (dosync
    (alter *spells* conj spell)))

(defn heal []
  (dosync
    (alter *health* + 3))
  "Вы исцелились на 3 единицы здоровья.")

(defn move-between-refs
  "Переместить объект из одного рефа в другой. Должен вызываться в транзакции."
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

(defn teleport []
  (let [rooms (keys @rooms/rooms)
        target-room (rand-nth rooms)]
    (dosync
      (move-between-refs *name* (:inhabitants @*current-room*) (:inhabitants (@rooms/rooms target-room)))
      (ref-set *current-room* (@rooms/rooms target-room)))
    (str "Вы телепортировались в " (name target-room) ".")))

(defn use-spell [spell monster]
  (if (and (contains? @*spells* spell) (> @*mana* 0))
    (let [spell-effect (case spell
                         :fireball 3
                         :ice_spike 2
                         :lightning 4
                         :heal (do (heal) 0)
                         :teleport (do (teleport) 0))]
      (dosync
        (alter *mana* - 1))
      spell-effect)
    "У вас нет маны для использования этого заклинания."))

(defn decrement-invisibility []
  (when (> @*invisible-turns* 0)
    (dosync
      (alter *invisible-turns* - 1))))

(defn apply-poison-damage []
  (when @*poisoned*
    (dosync
      (alter *health* - 1))
    (println "Вы получаете урон от яда.")))

(defn reward-player []
  "Вы получили награду!")

(defn resurrect []
  (dosync
    (alter *resurrect* (constantly true))))