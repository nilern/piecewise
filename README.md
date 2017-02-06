
    example = @mod grid life <- {
        grid = @mod make rows cols ref each set <- {
            make n m = into [] (repeat n (repeat m false));
            
            rows grid = count grid;

            cols grid = count (grid 0);

            ref grid n m = grid 0 0;

            set grid n m v = assocIn grid [n, m] v;

            each grid f = fmap (fmap f) grid
        };
    
        life = @mod life <- {
            lifeCount grid i j = {
                count i j = @match (ref grid i j) {
                    (:some, _) -> 1;
                    :none -> 0
                };

                sum (@for {di <- (-1, 0, 1); dj <- (-1, 0, 1)} count i j)
            };

            lifeAlive? grid i j = @match (lifeCount grid i j) {
                3 -> true;
                2 -> ref grid i j;
                _ -> false
            }

            # ...
        }
    };

    grid = @-> (example.grid.make 24 24)
               (example.grid.set 1 1 true)
               (example.grid.set 2 2 true) 
               (example.grid.set 3 0 true)
               (example.grid.set 3 1 true)
               (example.grid.set 3 2 true)

    example.life.life grid 80
            