module Error where

import Pretty
import SrcLoc

import Control.Exception.Safe
import Control.Monad.IO.Class

----------------------------------------------------------------
-- General Error
----------------------------------------------------------------
eitherToMonadThrow :: (MonadThrow m, Exception e) => Either e a -> m a
eitherToMonadThrow (Left e) = throw e
eitherToMonadThrow (Right a) = return a

catchError :: (MonadCatch m, MonadIO m) => m () -> m ()
catchError =
        ( `catches`
                [ Handler $ \e@PsError{} -> liftIO $ putStrLn $ pretty e
                ]
        )

----------------------------------------------------------------
-- Parser Error
----------------------------------------------------------------
data PsError = PsError {errorMessage :: String, errorSpan :: Span} deriving (Show)

instance Pretty PsError where
        pretty (PsError msg (Span s e)) = pretty s ++ "-" ++ pretty e ++ ": " ++ msg

instance Exception PsError

throwPsError :: MonadThrow m => Span -> String -> m a
throwPsError sp msg = throw $ PsError msg sp