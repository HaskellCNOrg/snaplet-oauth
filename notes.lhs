------------------------------------------------------------------------------
--                    Working notes                                   --
------------------------------------------------------------------------------

-- modifySnapletState :: (Snaplet v -> Snaplet v) -> Handler b v ()
-- getL :: (Lens a b) -> a -> b
-- setL :: (Lens a b) -> b -> a -> a
-- modL :: (Lens a b) -> (b -> b) -> a -> a
-- app :: SnapletInit App App
-- snapletConfig :: Lens (Snaplet a) SnapletConfig
-- snapletValue :: Lens (Snaplet a) a

{-
 getMongoDB :: app -> MongoDB
 getMongoDB = getL (snapletValue . database)

-- | translate as
--   MonadState App (Handler App App)
--   thus: `get` could be used for getting `App` instances inside `Handler b v` monad.
-- 
instance MonadState v (Handler b v) where
    get = getsSnapletState _snapletValue
    put v = modifySnapletState (setL snapletValue v)


--
with
withTop
  at first grance, there have same type signature. 
  so any differences?? dig into detail but not understand it yet.
  *****
  ** It means current snaplet or the 'top/parent' snaplet
  ** 'top or parent' means when current snaplet used by snaplet A,
  ** A becomes 'top' snaplet.
  *****

with
  :: MonadSnaplet m
     => Data.Lens.Common.Lens v (Snaplet v')
     -> m b v' a
     -> m b v a

addRoutes :: [(ByteString, Handler b v ())]
               -> Initializer b v ()

makeSnapletSource
:: Text	
  -> Text	
  -> Maybe (IO FilePath)	
  -> Initializer b v v	
  -> SnapletInit b v

-}
