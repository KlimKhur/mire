(ns mire.server
  (:require [clojure.java.io :as io]
            [server.socket :as socket]
            [mire.player :as player]
            [mire.commands :as commands]
            [mire.rooms :as rooms]))

(defn- cleanup []
  "Drop all inventory and remove player from room and player list."
  (dosync
   (doseq [item @player/*inventory*]
     (commands/discard item))
   (commute player/streams dissoc player/*name*)
   (commute (:inhabitants @player/*current-room*) disj player/*name*)
   (println "Соединение завершено."))
  (System/exit 0)) ; Закрываем соединение с текущим клиентом


(defn- get-unique-player-name [name]
  (if (@player/streams name)
    (do (print "That name is in use; try again: ")
        (flush)
        (recur (read-line)))
    name))

(defn- mire-handle-client [in out]
  (binding [*in* (io/reader in)
            *out* (io/writer out)
            *err* (io/writer System/err)]
    (try
      ;; Мы должны вложить это в другой вызов binding, а не использовать
      ;; тот, что выше, чтобы *in* и *out* были привязаны к сокету
      (print "\nКак вас зовут? ") (flush)
      (binding [player/*name* (get-unique-player-name (read-line))
                player/*current-room* (ref (@rooms/rooms :start))
                player/*inventory* (ref #{})
                player/*equipment* (ref #{})
                player/*health* (ref 5)
                player/*mana* (ref 5)
                player/*spells* (ref #{})
                player/*visited-rooms* (ref 0)
                player/*invisible-turns* (ref 0)
                player/*resurrect* (ref false)
                player/*poisoned* (ref false)]
        (dosync
         (commute (:inhabitants @player/*current-room*) conj player/*name*)
         (commute player/streams assoc player/*name* *out*))

        (println (commands/look)) (print player/prompt) (flush)

        (loop [input (read-line)]
          (when input
            (println (commands/execute input))
            (.flush *err*)
            (print player/prompt) (flush)
            (recur (read-line)))))
      (catch Exception e
        (.printStackTrace e (new java.io.PrintWriter *err*)))
      (finally (cleanup))))) ; Завершаем соединение в блоке finally


(defn -main
  ([port dir]
     (rooms/add-rooms dir)
     (defonce server (socket/create-server (Integer. port) mire-handle-client))
     (println "Launching Mire server on port" port))
  ([port] (-main port "resources/rooms"))
  ([] (-main 3333)))