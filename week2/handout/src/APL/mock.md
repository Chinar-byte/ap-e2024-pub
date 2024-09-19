## Report

##### Exercise One - Print

The most interesting part of extending the type definition of the EvalM type is manipulating the monad and the applicator instances to be able to handle the new 'printing' array within a tuple. I would say the quality of code resulting from this is fairly high - substantiated by correct outputs from extensive testing and high code readability.
We also found that the implementation of this method of 'printing' proved to be highly helpful in debugging various other methods and functions we implemented after - further improving the readability of the code. I also appreciate the cleanliness of the code that arises from utilising the monadic implementation of the code.

For example the following code blocks

```
instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \env store -> ([], Right x)
  (<*>) = ap
```

The use of liftM and ap functions defined by Haskell rather than a long definition improves the readability of the code drastically.

##### Exercise Two

This is the hardest exercise that we completed in the assignment. In particular implementing the State monad structure which altered the values stores in the 'Store' using the KvPut value was incredibly difficult. An interesting note is that if we were allowed to edit the type of that function from Val -> Val -> EvalM () to Val -> Val -> EvalM Val, this would allow us to return the updated store easier. However I do understand that the current implementation follows the state monad.

Additionally the lazy evaluation of Haskell makes it harder to compute functions and instructions where the result is irrelevant but the computation is necessary.

Whilst this exercise was easy to test and attain a high coverage alluding to a better quality of code - the implementation at the moment leaves a lot to be desired. I believe the code is not as clean as it can be. The monadic implementation already cleans up a fair amount of code - but considering the difficulty we had in implementing the KvPut function - I believe the overall functionality can be cleaner.

##### Exercise Three

This exercise was an implementation
