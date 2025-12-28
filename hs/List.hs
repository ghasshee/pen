
data List a = Nil | Cons a (List a) 


foldList l c n = case l of 
            Nil -> n 
            Cons a x -> c a x 
