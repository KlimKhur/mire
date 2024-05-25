(ns mire.commands
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [mire.rooms :as rooms]
            [mire.player :refer [*inventory* *equipment* *current-room* *name* *health* *mana* *spells* *visited-rooms* *invisible-turns* *resurrect* *poisoned* carrying? equipped? inventory-full? equipment-full? prompt streams take-damage attack learn-spell use-spell reward-player decrement-invisibility apply-poison-damage resurrect heal teleport]]))

(def monsters {"ghosts" {:health 3 :attack 1 :weakness :spell_book :afraid-of-torch true}
               "goblins" {:health 2 :attack 2 :weakness :blazing_sword :afraid-of-torch false}
               "giant_spiders" {:health 4 :attack 3 :weakness :torch :poisonous true :afraid-of-torch true}
               "skeletons" {:health 3 :attack 2 :weakness :thunder_hammer :afraid-of-torch false}
               "zombie" {:health 5 :attack 2 :weakness :torch :afraid-of-torch false}
               "ogres" {:health 5 :attack 4 :weakness :dragon_bow :afraid-of-torch false}
               "shadow_knights" {:health 6 :attack 5 :weakness :blazing_sword :afraid-of-torch false}
               "stone_golems" {:health 7 :attack 4 :weakness :spell_book :afraid-of-torch false}
               "dragon" {:health 10 :attack 7 :weakness :dragon_bow :afraid-of-torch false}
               "vampire" {:health 4 :attack 3 :weakness :torch :afraid-of-torch true}})

(def weak-monsters #{"ghosts" "goblins" "giant_spiders" "skeletons" "zombie"})
(def strong-monsters #{"ogres" "shadow_knights" "stone_golems" "dragon" "vampire"})

(def common-loot [:healing_potion :antidote :torch :key :bandages :shield :helmet :food_rations :water_flask])
(def rare-loot [:magic_sword :enchanted_armor :spell_book :golden_crown :ancient_relic :dragon_scale :phoenix_feather :elven_cloak :mystic_amulet :blazing_sword :thunder_hammer :dragon_bow :shadow_dagger :titan_axe])

(defn move-between-refs
  "Переместить объект из одного рефа в другой. Должен вызываться в транзакции."
  [obj from to]
  (when (and (some? from) (some? to))
    (alter from disj obj)
    (alter to conj obj)))

(defn look
  "Получить описание окружающей обстановки и её содержимого."
  []
  (println (:desc @*current-room*))
  (println "Выходы: " (keys @(:exits @*current-room*)))
  (when-let [items (seq @(:items @*current-room*))]
    (println "Здесь лежат:" (str/join ", " (map name items))))
  (when-let [enemies (seq @(:inhabitants @*current-room*))]
    (let [monster-names (filter #(contains? monsters (name %)) enemies)]
      (when (seq monster-names)
        (println "Монстры в комнате:" (str/join ", " (map name monster-names))))))
  (flush))

(defn give-all-spells []
  (dosync
    (alter *spells* conj :fireball :ice_spike :lightning :heal :teleport))
  (println "Вы получили все заклинания.")
  (flush))

(defn equip
  ([]
   (println "Пожалуйста, укажите предмет, который хотите экипировать.") (flush))
  ([thing]
   (dosync
    (if (carrying? thing)
      (case (keyword thing)
        :magic_sword (do
                       (move-between-refs (keyword thing) *inventory* *equipment*)
                       (println "Вы экипировали Волшебный меч и теперь наносите больше урона.") (flush))
        :enchanted_armor (do
                           (move-between-refs (keyword thing) *inventory* *equipment*)
                           (println "Вы надели Заколдованные доспехи и теперь получаете меньше урона.") (flush))
        :golden_crown (do
                        (move-between-refs (keyword thing) *inventory* *equipment*)
                        (println "Вы надели Золотую корону и теперь получаете бонус к защите.") (flush))
        :dragon_scale (do
                        (move-between-refs (keyword thing) *inventory* *equipment*)
                        (println "Вы надели Чешую дракона и теперь получаете огромный бонус к защите.") (flush))
        :blazing_sword (do
                         (move-between-refs (keyword thing) *inventory* *equipment*)
                         (println "Вы экипировали Пылающий меч и теперь наносите больше урона.") (flush))
        :thunder_hammer (do
                          (move-between-refs (keyword thing) *inventory* *equipment*)
                          (println "Вы экипировали Громовой молот и теперь наносите больше урона.") (flush))
        :dragon_bow (do
                      (move-between-refs (keyword thing) *inventory* *equipment*)
                      (println "Вы экипировали Драконий лук и теперь наносите больше урона.") (flush))
        :shadow_dagger (do
                         (move-between-refs (keyword thing) *inventory* *equipment*)
                         (println "Вы экипировали Кинжал тени и теперь наносите больше урона.") (flush))
        :titan_axe (do
                     (move-between-refs (keyword thing) *inventory* *equipment*)
                     (println "Вы экипировали Топор титана и теперь наносите больше урона.") (flush))
        :torch (do
                 (move-between-refs (keyword thing) *inventory* *equipment*)
                 (println "Вы зажгли факел и теперь можете отпугивать некоторых монстров.") (flush))
        :healing_potion (do (println "Вы не можете экипировать зелье исцеления.") (flush))
        :antidote (do (println "Вы не можете экипировать антидот.") (flush))
        :key (do (println "Вы не можете экипировать ключ.") (flush))
        :bandages (do (println "Вы не можете экипировать бинты.") (flush))
        :food_rations (do (println "Вы не можете экипировать паек.") (flush))
        :water_flask (do (println "Вы не можете экипировать флягу с водой.") (flush))
        :spell_book (do (println "Вы не можете экипировать книгу заклинаний.") (flush))
        (do (println (str "Вы не можете экипировать " thing ".")) (flush)))
      (do (println (str "У вас нет " thing ".")) (flush))))))

(defn unequip
  ([] (println "Пожалуйста, укажите предмет, который хотите снять.") (flush))
  ([thing]
   (dosync
    (if (equipped? thing)
      (if (inventory-full?)
        (do (println "Ваш инвентарь полон.") (flush))
        (do (move-between-refs (keyword thing)
                               *equipment*
                               *inventory*)
            (println (str "Вы сняли " thing ".")) (flush)))
      (do (println (str "У вас нет " thing ".")) (flush))))))

(defn handle-attack [monster monster-health]
  (let [damage-to-monster (attack monster)]
    (alter monster-health - damage-to-monster)
    (println (str "Вы нанесли " damage-to-monster " урона " monster "."))
    (if (pos? @monster-health)
      (let [afraid-of-torch (get-in monsters [monster :afraid-of-torch])
            has-torch (equipped? :torch)
            damage-to-player (if (and afraid-of-torch has-torch)
                               (do
                                 (println (str monster " боится вашего факела и не атакует вас!"))
                                 0)
                               (if (zero? @*invisible-turns*)
                                 (take-damage (get-in monsters [monster :attack]))
                                 0))]
        (when (pos? damage-to-player)
          (println (str monster " нанес вам " damage-to-player " урона."))
          (when (and (get-in monsters [monster :poisonous]) (not @*poisoned*))
            (dosync (alter *poisoned* (constantly true)))
            (println (str "Вы отравлены " monster "!"))))
        (if (pos? @*health*)
          :continue
          :break))
      (do
        (if (= monster "dragon")
          (do
            (println "***************************************************")
            (println "*                                                 *")
            (println "*  Поздравляем! Вы убили дракона и освободили      *")
            (println "*  принцессу из его лап! Ваши подвиги будут        *")
            (println "*  воспеты в веках, и ваше имя станет легендой!    *")
            (println "*                                                 *")
            (println "*  Игра окончена. Спасибо за игру!                 *")
            (println "*                                                 *")
            (println "***************************************************")
            (System/exit 0))
          (do
            (println (str "Вы победили " monster "! Ваше здоровье: " @*health* "/5, Мана: " @*mana* "/5"))
            :break))))))

(defn escape []
  (let [exits (keys @(:exits @*current-room*))]
    (if (empty? exits)
      (do
        (println "Здесь нет выхода, вы не можете сбежать!")
        (flush))
      (let [direction (rand-nth exits)
            target-name (get @(:exits @*current-room*) direction)
            target (@rooms/rooms target-name)]
        (dosync
         (move-between-refs *name*
                            (:inhabitants @*current-room*)
                            (:inhabitants target))
         (ref-set *current-room* target))
        (println (str "Вы сбежали в " (name direction) "!"))
        (println "Вы сбежали из боя!")
        (println (look))
        (flush)
        :break))))

(defn handle-taunt [monster monster-health]
  (println (str "Вы насмехаетесь над " monster ". Это раздражает его, и он атакует вас!"))
  (let [damage-to-player (if (zero? @*invisible-turns*)
                           (take-damage (get-in monsters [monster :attack]))
                           0)]
    (when (pos? damage-to-player)
      (println (str monster " нанес вам " damage-to-player " урона."))
      (when (and (get-in monsters [monster :poisonous]) (not @*poisoned*))
        (dosync (alter *poisoned* (constantly true)))
        (println (str "Вы отравлены " monster "!")))))
  (if (and (pos? @monster-health) (pos? @*health*))
    :continue
    :break))

(defn handle-equip [monster monster-health]
  (println "Какой предмет вы хотите экипировать?")
  (println "Доступные предметы для экипировки:")
  (println (str/join "\n" (seq @*inventory*)))
  (print "> ") (flush)
  (let [item (read-line)]
    (println (equip item)))
  (if (and (pos? @monster-health) (pos? @*health*))
    :continue
    :break))

(defn handle-unequip [monster monster-health]
  (println "Какой предмет вы хотите снять?")
  (println "Доступные предметы для снятия:")
  (println (str/join "\n" (seq @*equipment*)))
  (print "> ") (flush)
  (let [item (read-line)]
    (println (unequip item)))
  (if (and (pos? @monster-health) (pos? @*health*))
    :continue
    :break))

(defn handle-cast-spell [monster monster-health]
  (println "Какое заклинание вы хотите использовать?")
  (println "Доступные заклинания:")
  (println (str/join "\n" (seq @*spells*)))
  (print "> ") (flush)
  (let [spell (read-line)
        spell-key (keyword spell)]
    (if (contains? @*spells* spell-key)
      (let [spell-result (use-spell spell-key monster)]
        (cond
          (string? spell-result)
          (do
            (println spell-result)
            :continue)
          
          (= spell-key :teleport)
          (if (>= @*mana* 3)
            (do
              (alter *mana* - 3)
              (println "Вы телепортировались в другую комнату и избегли боя.")
              :break)
            (do
              (println "У вас нет маны для использования этого заклинания.")
              :continue))
          
          (= spell-key :heal)
          (if (>= @*mana* 2)
            (do
              (alter *mana* - 2)
              (let [heal-amount 3]
                (alter *health* #(min 5 (+ % heal-amount)))
                (println (str "Вы использовали заклинание исцеления и восстановили " heal-amount " здоровья."))
                (println (str "Ваше здоровье: " @*health* "/5, Мана: " @*mana* "/5"))
                :continue))
            (do
              (println "У вас нет маны для использования этого заклинания.")
              :continue))
          
          :else
          (let [damage-to-monster spell-result]
            (alter monster-health - damage-to-monster)
            (println (str "Вы нанесли " damage-to-monster " урона " monster " заклинанием " spell "."))
            (if (pos? @monster-health)
              (let [damage-to-player (if (zero? @*invisible-turns*)
                                       (take-damage (get-in monsters [monster :attack]))
                                       0)]
                (when (pos? damage-to-player)
                  (println (str monster " нанес вам " damage-to-player " урона.")))
                (println (str "Ваше здоровье: " @*health* "/5, Мана: " @*mana* "/5, Здоровье монстра: " @monster-health))
                :continue)
              (do
                (if (= monster "dragon")
                  (do
                    (println "***************************************************")
                    (println "*                                                 *")
                    (println "*  Поздравляем! Вы убили дракона и освободили      *")
                    (println "*  принцессу из его лап! Ваши подвиги будут        *")
                    (println "*  воспеты в веках, и ваше имя станет легендой!    *")
                    (println "*                                                 *")
                    (println "*  Игра окончена. Спасибо за игру!                 *")
                    (println "*                                                 *")
                    (println "***************************************************")
                    (System/exit 0))
                  (println (str "Вы победили " monster "! Ваше здоровье: " @*health* "/5, Мана: " @*mana* "/5")))
                :break)))))
      (do
        (println "У вас нет такого заклинания.")
        :continue))))

(defn start-fight
  "Начать бой с монстром."
  [monster]
  (if (contains? monsters monster)
    (let [monster-health (ref (get-in monsters [monster :health]))]
      (loop [continue? true]
        (when (and continue? (pos? @monster-health) (pos? @*health*))
          (println "Ваш ход. Доступные действия: атаковать, насмехнуться, экипировать, снять, использовать заклинание, сбежать.")
          (print "> ") (flush)
          (let [action (read-line)
                result (case action
                         "атаковать" (handle-attack monster monster-health)
                         "насмехнуться" (handle-taunt monster monster-health)
                         "экипировать" (handle-equip monster monster-health)
                         "снять" (handle-unequip monster monster-health)
                         "использовать заклинание" (handle-cast-spell monster monster-health)
                         "сбежать" (escape)
                         (do
                           (println "Неверное действие. Пожалуйста, выберите одно из доступных действий.")
                           :continue))]
            (if (= result :break)
              (do
                (if (<= @*health* 0)
                  (do
                    (println "***************************************************")
                    (println "*                                                 *")
                    (println "*  Вы были побеждены! Ваши приключения подошли     *")
                    (println "*  к концу. Попробуйте снова и будьте осторожнее!  *")
                    (println "*                                                 *")
                    (println "*  Игра окончена. Спасибо за игру!                 *")
                    (println "*                                                 *")
                    (println "***************************************************")
                    (System/exit 0))
                  (when (pos? @monster-health)
                    (println (str "Ваше здоровье: " @*health* "/5, Мана: " @*mana* "/5, Здоровье монстра: " @monster-health))))
                (recur false)) ; Завершаем цикл
              (recur (and (pos? @monster-health) (pos? @*health*))))))))
    (println "Здесь нет такого монстра.")))

(defn move
  "\"♬ Нам надо выбраться отсюда... ♪\" Укажите направление."
  ([]
   (println "Пожалуйста, укажите направление для перемещения. Например, 'move north'.")
   (flush))
  ([direction]
   (dosync
    (let [target-name (get @(:exits @*current-room*) (keyword direction))
          target (@rooms/rooms target-name)]
      (if target
        (do
          (move-between-refs *name*
                             (:inhabitants @*current-room*)
                             (:inhabitants target))
          (ref-set *current-room* target)
          (alter *visited-rooms* inc)
          (decrement-invisibility)
          (let [description (look)
                current-enemies (filter #(contains? monsters (name %)) @(:inhabitants target))
                current-items @(:items target)]
            (println description)
            (flush)
            (if (seq current-enemies)
              (do
                (println (str "Вы видите " (str/join ", " (map name current-enemies)) " в этой комнате."))
                (flush)
                (when (< (rand) 0.5)
                  (let [monster (rand-nth (vec current-enemies))]
                    (println (str "Вы столкнулись с " monster "! Бой начинается!"))
                    (flush)
                    (start-fight monster))))
              (do
                (when (< (rand) 0.5)
                  (let [monster (if (< @*visited-rooms* 10)
                                  (rand-nth (vec weak-monsters))
                                  (rand-nth (vec (set/union weak-monsters strong-monsters))))]
                    (println (str "Вы столкнулись с " monster "! Бой начинается!"))
                    (flush)
                    (start-fight monster)))
                (when (and (empty? current-items) (< (rand) 0.5))
                  (let [item (if (< @*visited-rooms* 10)
                               (rand-nth common-loot)
                               (rand-nth (concat common-loot rare-loot)))]
                    (println (str "Вы нашли " (name item) " на полу!"))
                    (flush)
                    (dosync
                      (alter (:items target) conj item))))
                (println "Вы переместились в новую комнату.")
                (flush))))
        (do
          (println "О, игра работает!")
          (flush)
          "Поставьте зачет")))))))

(defn grab
  "Поднять предмет."
  ([] (println "Пожалуйста, укажите предмет, который хотите подобрать.") (flush))
  ([thing]
   (dosync
    (if (rooms/room-contains? @*current-room* thing)
      (if (inventory-full?)
        (do (println "Ваш инвентарь полон.") (flush))
        (do (move-between-refs (keyword thing)
                               (:items @*current-room*)
                               *inventory*)
            (println (str "Вы подобрали " thing ".")) (flush)))
      (do (println (str "Здесь нет " thing ".")) (flush))))))

(defn discard
  "Выбросить предмет из инвентаря."
  ([] (println "Пожалуйста, укажите предмет, который хотите выбросить.") (flush))
  ([thing]
   (dosync
    (if (carrying? thing)
      (do (move-between-refs (keyword thing)
                             *inventory*
                             (:items @*current-room*))
          (println (str "Вы выбросили " thing ".")) (flush))
      (do (println (str "У вас нет " thing ".")) (flush))))))

(defn inventory
  "Показать инвентарь."
  []
  (println "Ваш инвентарь:")
  (println (str/join "\n" (seq @*inventory*)))
  (println (str "Места в инвентаре: " (count @*inventory*) "/10")) (flush))

(defn equipment
  "Показать экипировку."
  []
  (println "Ваша экипировка:")
  (println (str/join "\n" (seq @*equipment*)))
  (println (str "Места в экипировке: " (count @*equipment*) "/6")) (flush))

(defn use-item
  "Использовать предмет из инвентаря."
  ([] 
    (do
      (println "Пожалуйста, укажите предмет, который хотите использовать.") 
      (flush)))
  ([thing]
   (if (carrying? thing)
     (do
       (case (keyword thing)
         :healing_potion (do
                           (dosync
                            (alter *inventory* disj (keyword thing)))
                           (dosync
                            (alter *health* #(min 5 (+ % 2))))
                           (println "Вы использовали зелье исцеления и восстановили здоровье.") 
                           (flush))
         :antidote (do
                    (dosync
                     (alter *inventory* disj (keyword thing)))
                    (dosync
                     (alter *poisoned* (constantly false)))
                    (println "Вы использовали антидот и излечились от яда.") 
                    (flush))
         :torch (do
                  (println "Вы не можете использовать факел как предмет.") 
                  (flush))
         :key (do
                (dosync
                 (alter *inventory* disj (keyword thing)))
                (println "Вы использовали ключ для открытия двери.") 
                (flush))
         :bandages (do
                     (dosync
                      (alter *inventory* disj (keyword thing)))
                     (dosync
                      (alter *health* #(min 5 (+ % 1))))
                     (println "Вы использовали бинты и восстановили немного здоровья.") 
                     (flush))
         :food_rations (do
                         (dosync
                          (alter *inventory* disj (keyword thing)))
                         (dosync
                          (alter *health* #(min 5 (+ % 1))))
                         (println "Вы съели паек и восстановили здоровье.") 
                         (flush))
         :water_flask (do
                        (dosync
                         (alter *inventory* disj (keyword thing)))
                        (dosync
                         (alter *health* #(min 5 (+ % 1))))
                        (println "Вы выпили воды и восстановили здоровье.") 
                        (flush))
         :magic_sword (do
                        (println "Вы не можете использовать Волшебный меч как предмет.") 
                        (flush))
         :enchanted_armor (do
                           (println "Вы не можете использовать Заколдованные доспехи как предмет.") 
                           (flush))
         :golden_crown (do
                        (println "Вы не можете использовать Золотую корону как предмет.") 
                        (flush))
         :dragon_scale (do
                        (println "Вы не можете использовать Чешую дракона как предмет.") 
                        (flush))
         :blazing_sword (do
                         (println "Вы не можете использовать Пылающий меч как предмет.") 
                         (flush))
         :thunder_hammer (do
                          (println "Вы не можете использовать Громовой молот как предмет.") 
                          (flush))
         :dragon_bow (do
                      (println "Вы не можете использовать Драконий лук как предмет.") 
                      (flush))
         :shadow_dagger (do
                         (println "Вы не можете использовать Кинжал тени как предмет.") 
                         (flush))
         :titan_axe (do
                     (println "Вы не можете использовать Топор титана как предмет.") 
                     (flush))
         :spell_book (do
                       (dosync
                        (alter *inventory* disj (keyword thing)))
                       (let [spell (rand-nth (vec #{:fireball :ice_spike :lightning :heal :teleport}))]
                         (dosync
                          (alter *spells* conj spell))
                         (println (str "Вы изучили новое заклинание: " (name spell) ".")) 
                         (flush)))
         (do
           (println (str "Вы не можете использовать " thing ".")) 
           (flush)))
       nil)
     (do
       (println (str "У вас нет " thing ".")) 
       (flush)))))


(defn say
  "Сказать что-то, чтобы все в комнате услышали."
  [& words]
  (let [message (str/join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @*current-room*) *name*)]
      (binding [*out* (streams inhabitant)]
        (println message)
        (println prompt) (flush)))
    (str "Вы сказали: " message)))

(defn stats
  "Показать текущие значения здоровья и маны."
  []
  (println (str "Здоровье: " @*health* "/5")) (flush)
  (println (str "Мана: " @*mana* "/5")) (flush))

(defn spells
  "Показать список изученных заклинаний."
  []
  (println "Ваши заклинания:") (flush)
  (println (str/join "\n" (seq @*spells*))) (flush))

(defn help
  "Показать доступные команды и их описание."
  []
  (println "Доступные команды:")
  (println "move: Переместиться в указанном направлении (например, 'move north').")
  (println "grab: Поднять предмет (например, 'grab torch').")
  (println "discard: Выбросить предмет из инвентаря (например, 'discard torch').")
  (println "inventory: Показать инвентарь.")
  (println "equip: Экипировать предмет (например, 'equip torch').")
  (println "unequip: Снять экипированный предмет (например, 'unequip torch').")
  (println "equipment: Показать экипировку.")
  (println "use: Использовать предмет из инвентаря (например, 'use healing_potion').")
  (println "look: Получить описание окружающей обстановки и её содержимого.")
  (println "say: Сказать что-то, чтобы все в комнате услышали.")
  (println "stats: Показать текущие значения здоровья и маны.")
  (println "spells: Показать список изученных заклинаний.")
  (println "help: Показать доступные команды и их описание."))

;; Команды

(def commands {"move" move,
               "north" (fn [] (move :north)),
               "south" (fn [] (move :south)),
               "east" (fn [] (move :east)),
               "west" (fn [] (move :west)),
               "grab" grab,
               "discard" discard,
               "inventory" inventory,
               "equip" equip,
               "unequip" unequip,
               "equipment" equipment,
               "use" use-item,
               "look" look,
               "say" say,
               "stats" stats,
               "spells" spells,
               "start-fight" start-fight,
               "help" help,
               "give-all-spells" give-all-spells})

;; Выполнение команд

(defn execute
  "Выполнить команду, переданную пользователем."
  [input]
  (try 
    (when (<= @*health* 0)
      (println "Вы мертвы. Игра окончена.")
      (System/exit 0))
    (let [[command & args] (.split input " +")
          f (commands command)]
      (if f
        (if (empty? args)
          (f)
          (apply f args))
        (do
          (println (str "Неизвестная команда: " command)) (flush))))
    (catch Exception e
      (.printStackTrace e (new java.io.PrintWriter *err*))
      (println "Произошла ошибка при выполнении команды!") (flush))))