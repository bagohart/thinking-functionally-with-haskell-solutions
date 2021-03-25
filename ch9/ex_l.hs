type Matrix a = [[a]]

data Torus a = Cell a (Torus a) (Torus a)
                      (Torus a) (Torus a)

elem  (Cell a u d l r) = a
up    (Cell a u d l r) = u
down  (Cell a u d l r) = d
left  (Cell a u d l r) = l
right (Cell a u d l r) = r

mkTorus :: Matrix a -> Torus a
mkTorus = undefined

-- ... I should probably tackle this exercise AFTER reading the section about doubly linked lists...
