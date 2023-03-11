||| Inspired by Zig's errdefer keyword: https://ziglang.org/learn/overview/#manual-memory-management
module ErrDefer
import Control.Monad.Trans

export
data Deferrable: Type -> (Type -> Type) -> Type -> Type where
   NotDeferred: m (Either e a) -> Deferrable e m a
   Deferred:          m ()     -> Deferrable e m ()
   ErrDeferred: (e -> m ())    -> Deferrable e m ()

export
Functor m => Functor (Deferrable e m) where
   map = ?defer_map
export
Applicative m => Applicative (Deferrable e m) where
   pure = NotDeferred . pure . Right
   (<*>) = ?defer_apply
export
Monad m => Monad (Deferrable e m) where
   (>>=) = ?defer_bind

||| Executes a sequence that may include deferred operations.
public export runDeferrable: Deferrable e m a -> m (Either e a)
||| Lifts an operation with a typical Idris error type into the Deferrable monad.
public export try: Cast e' e => m (Either e' a) -> Deferrable e m a
||| Defers an operation to happen only if the sequence encounters an error.
public export errdefer: m () -> Deferrable e m a
||| Yields an error. Deferred operations will execute prior to returning the error value.
public export error: e -> Deferrable e m ()
||| Defers a computation to the end of the sequence.
public export defer: m () -> Deferrable e m ()
||| Bracketing a deferrable sequence causes deferred operations to run at the end of the subsequence
||| rather than at the end of the whole sequence.
public export bracket: Deferrable e m a -> Deferrable e m a
bracket inner = NotDeferred (runDeferrable inner)

export
MonadTrans (Deferrable e) where
   lift = NotDeferred . map Right


namespace Example

   export
   data AllocatorError: Type
   record Allocator where
      constructor MkAllocator
      create: (t: Type) -> IO (Either AllocatorError t)
      destroy: {t: Type} -> t -> IO ()

   export
   fmt: Allocator -> String -> List String -> IO (Either AllocatorError String)

   public export
   data CreateError = CreateFailedAlloc AllocatorError | ReservedDeviceId
   export
   Cast AllocatorError CreateError where
      cast = CreateFailedAlloc

   ||| Allocates an object that is always destroyed at the end of the sequence.
   export
   (.scoped): Allocator -> (t: Type) -> Deferrable AllocatorError IO t
   (.scoped) allocator t = do
      object <- try (allocator.create t)
      defer (allocator.destroy object)
      pure object

   public export
   record Device where
      constructor MkDevice
      setName: String -> IO ()

   export
   createDevice: Allocator -> Int -> IO (Either CreateError Device)
   createDevice allocator id = runDeferrable$ do
      when (id == 0) (error ReservedDeviceId)
      device <- try (allocator.create Device)
      errdefer (allocator.destroy device)
      name <- try (fmt allocator "Device(id={d})" [show id])
      errdefer (allocator.destroy name)
      lift$ device.setName name
      pure device

