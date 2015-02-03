data IList a = Nil | Cons a (IVar (IList a))

type Stream a = IVar (IList a)

streamFromList :: NFData a => [a] -> Par (Stream a)
streamFromList xs = do
    var <- new
    fork $ loop xs var
    return var
    where
        loop [] var = put var Nil
        loop (x:xs) var = do
            tail <- new
            put var (Cons x tail)
            loop xs tail

streamFold :: (a -> b -> c) -> a -> Stream b -> Par a
streamFold fn !acc instream = do
    ilist <- get instream
    case ilist of
        Nil -> return acc
        Cons head tail -> streamFold fn (fn acc head) tail

streamMap :: NFData b => (a -> b) -> Stream a -> Par (Stream b)
streamMap fn instream = do
    outstream <- new
    fork $ loop instream outstream
    return outstream
    where
        loop instream outstream = do 
        ilist <- get instream
        case ilist of
            Nil -> put outstream Nil
            Cons head tail -> do
                newtail <- new
                put outstream (Cons (fn head) newtail)
                loop tail newtail




