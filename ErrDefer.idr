||| Inspired by Zig's errdefer keyword
module ErrDefer
import Control.Monad.Trans

--data Deferrable: (Type -> Type)

data HashMap: Type
data Err: Type
record Device where
   constructor MkDevice
   setName: String -> IO ()

data AllocatorError: Type
record Allocator where
   constructor MkAllocator
   create: (t: Type) -> IO (Either AllocatorError t)
   destroy: {t: Type} -> t -> IO ()

--uninitializedHashMap: IO (Either Err HashMap)
--(.init): HashMap -> Allocator -> IO ()

interface ErrDefer (m: Type -> Type) where
    constructor MkErrDefer

data Deferrable: Type -> (Type -> Type) -> Type -> Type
Functor m => Functor (Deferrable e m) where
   map = ?defer_map
Applicative m => Applicative (Deferrable e m) where
   pure = ?defer_pure
   (<*>) = ?defer_apply
Monad m => Monad (Deferrable e m) where
   (>>=) = ?defer_bind

||| Executes a sequence that may include deferred operations.
public export runDefer: Deferrable e m a -> m (Either e a)
||| Lifts an operation with a typical Idris error type into the Deferrable monad.
public export try: Cast e' e => m (Either e' a) -> Deferrable e m a
||| Defers an operation to happen only if the sequence encounters an error.
public export errdefer: m () -> Deferrable e m a
||| Yields an error. Deferred operations will execute prior to returning the error value.
public export error: e -> Deferrable e m a
||| Defers a computation to the end of the sequence.
public export defer: m () -> Deferrable e m ()

MonadTrans (Deferrable e) where
   lift = ?defer_lift

--(>>=): ErrDefer m => m a -> (a -> m b) -> m b

fmt: Allocator -> String -> List String -> IO (Either AllocatorError String)

data CreateError = CreateFailedAlloc AllocatorError | ReservedDeviceId
Cast AllocatorError CreateError where
   cast = CreateFailedAlloc

createDevice: Allocator -> Int -> IO (Either CreateError Device)
createDevice allocator id = runDefer {m = IO}$ do
   when (id == 0) (error ReservedDeviceId)
   device <- try (allocator.create Device)
   errdefer (allocator.destroy device)
   name <- try (fmt allocator "Device(id={d})" [show id])
   errdefer (allocator.destroy name)
   lift$ device.setName name
   pure device

--test: IO (Either Err HashMap)
--test = runDefer$ do
--   labels <- uninitializedHashMap
--   labels.init allocator
--   errdefer labels.deinit
