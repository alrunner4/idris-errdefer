||| Inspired by Zig's errdefer keyword: https://ziglang.org/learn/overview/#manual-memory-management
module ErrDefer
import Control.Monad.Trans

export
data Deferrable: Type -> (Type -> Type) -> Type -> Type where
   DeferrableOperation: forall a, e, m, r.
      (Inf (a ->  Deferrable e m r) ->
           (deferred:         m ()) ->
           (errDeferred: e -> m ()) ->
         m (Either e r)) ->
      Deferrable e m a
   DeferrableValue: a -> Deferrable e m a

export
Functor m => Functor (Deferrable e m) where
   map f (DeferrableValue x)  = DeferrableValue (f x)
   map f (DeferrableOperation c) = ?map_operation
-- map f (DeferrableOperation k0 d0 e0) = DeferrableOperation (\k1 => k0 (\x => k1 (f x))) d0 e0

export
Applicative m => Applicative (Deferrable e m) where
   pure = DeferrableValue
   (<*>) = ?apply_deferrable

||| Handles errors, but doesn't run end-of-sequence deferred actions.
private
runDeferrable': forall a, e. Monad m => Deferrable e m a -> m (Either e a)
--runDeferrable' (DeferrableValue v) = pure (Right v)
--runDeferrable' (DeferrableOperation op ds es) = do
--   result <- op pure
--   case result of
--      Left err => for_ es ($ err) >> pure (Left err)
--      Right r  => pure (Right r)

export
Monad m => Monad (Deferrable e m) where
   DeferrableValue v >>= f = f v
   DeferrableOperation c >>= f = ?bind_operation
 --DeferrableOperation op0 d0 e0 >>= f = DeferrableOperation
 --   (\k1 => do
 --      result0 <- op0 pure
 --      case result0 of
 --         Left err => for_ e0 ($ err) >> pure (Left err)
 --         Right val => case f val of
 --            DeferrableValue v => runDeferrable' (k1 v)
 --            DeferrableOperation op1 d1 e1 => op1 k1)
 --   ?bind_deferred
 --   ?bind_errored

export
runDeferrable: forall a, e. Monad m => Deferrable e m a -> m (Either e a)
--runDeferrable (DeferrableValue v) = pure (Right v)
--runDeferrable (DeferrableOperation k ds es) = do
--   result <- k pure
--   case result of
--      Left err => do
--         for_ es ($ err)
--         sequence_ ds
--      Right r => sequence_ ds
--   pure result

-- DeferrablePure x >>= f = f x
-- DeferrableContinuation k >>= f = DeferrableContinuation$ \c,d,e => k
--    (\x => case f x of
--       DeferrablePure y => c ?y
--       DeferrableContinuation k' => ?bind_dc_cont )
--    ?bind_dc_d
--    ?bind_dc_e

   --MkDeferrable
   --   (\finally => d1.immediate (\r1 => case r1 of
   --      Left e1 => do
   --         traverse d1.errDeferred
   --   (do
   --      Right result1 <- d1.immediate
   --         | Left err1 => pure (Left err1)
   --      let d2 = fn result1
   --      Right result2 <- d2.immediate
   --         | Left err2 => pure (Left err2)
   --      pure (Right result2))
   --   ?bind_deferred
   --   ?bind_errdeferred

public export
interface ErrorConversion from to where
   convert: from -> to

public export
Cast from to => ErrorConversion from to where
   convert = cast

-- ErrorConversion child parent => ErrorConversion parent grandparent => ErrorConversion child grandparent where
--    convert = convert { from = parent, to = grandparent} . convert { from = child, to = parent }

public export
interface UniformError e where
   getModule:      e -> Int
   getDescription: e -> Int

public export
record AnyError where
   constructor AnyErrorDescriptor
   Module: Int
   Description: Int

UniformError e => ErrorConversion e AnyError where
   convert err = AnyErrorDescriptor (getModule err) (getDescription err)

-- ||| Executes a sequence that may include deferred operations.
-- public export runDeferrable: Deferrable e m a -> m (Either e a)
||| Lifts an operation with a typical Idris error type into the Deferrable monad.
public export try: ErrorConversion e' e => Functor m => m (Either e' a) -> Deferrable e m a
||| When the value of an error isn't needed, it can be erased.
public export eraseError: Functor m => m (Either e a) -> Deferrable () m a
||| Defers an operation to happen only if the sequence encounters an error.
public export errdefer: Applicative m => (e -> m ()) -> Deferrable e m ()
||| Yields an error. Deferred operations will execute prior to returning the error value.
public export error: Functor m => m e -> Deferrable e m ()
||| Defers a computation to the end of the sequence.
public export defer: Applicative m => m () -> Deferrable e m ()
||| Bracketing a deferrable sequence causes deferred operations to run at the end of the subsequence
||| rather than at the end of the whole sequence.
public export bracket: Deferrable e m a -> Deferrable e m a

public export mayError: ErrorConversion e' e => m (Maybe e') -> Deferrable e m ()
public export maySucceed: m (Maybe a) -> Deferrable () m a

--try op = MkDeferrable (\c,d,e => do
--   Right r <- op | Left err => e err
--   let MkDeferrable f = c r
--   f noop (pure ()) (const (pure ()))
   
   
-- defer op = MkDeferrable (\c => op >>= 

--bracket inner = NotDeferred (runDeferrable inner)
--defer    op      = DeferrableOperation (\k => ?defer_op) [op] []
--errdefer handler = MkDeferrable (pure (Right ()))  []  [handler]
--error    op      = DeferrableOperation (\k => map Left op)      []  []
--try op   = NotDeferred (map (mapFst cast) op)

--runDeferrable (NotDeferred op) = op
--runDeferrable (Deferred op) = ?rd_d

namespace Transformer

   export eraseError: Functor m => Deferrable e m a -> Deferrable () m a

namespace Simple

   export
   errdefer: Applicative m => m () -> Deferrable e m ()
   errdefer = errdefer . const

   export
   error: Applicative m => e -> Deferrable e m ()
   error = error . pure

export
MonadTrans (Deferrable e) where
   lift op = ?lift_deferrable


namespace Example

   export
   AllocatorError: Type
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

   public export
   Size: Type
   Size = Nat

   export RawMemoryRegion: Size -> Type

   namespace NN

      public export
      record MemoryResource where
         constructor DefineMemoryResource
         allocate: (bytes: Size) -> (alignment: Size) -> IO (Either AllocatorError (RawMemoryRegion bytes))
         deallocate: {bytes: Size} -> RawMemoryRegion bytes -> (alignment: Size) -> IO ()

      ||| Example: createExpHeap can embed deferred actions for error and normal exit cleanup,
      |||    so a separate interface for destroyExpHeap is unnecessary.
      createExpHeap: {sz: Size} -> ErrorConversion () e => RawMemoryRegion sz -> Deferrable e IO MemoryResource

      namespace Socket
         public export Error: Type
         public export ApiError: Type
         public export LibraryError: Type
         ErrorConversion Socket.Error AnyError
         ErrorConversion ApiError Socket.Error
         ErrorConversion LibraryError Socket.Error
         ErrorConversion AllocatorError Socket.Error
         ErrorConversion () Socket.Error
         public export data BufferDirection = Send | Receive | SendAndReceive
         public export data BufferAttribute = TcpInitialSize | TcpAutoTuneMax | UdpInitialSize
         public export
         record Api where
            constructor ApiMethods
            setSocketBufferAttribute: (BufferAttribute, BufferDirection, Size) -> IO (Maybe LibraryError)
         export initialize: Maybe MemoryResource -> IO (Either ApiError Api)

   export
   userAllocator: Allocator

   export
   userSocketAllocatorPoolSize: Size

   export
   userInitialize: IO (Maybe Socket.Error)
   userInitialize = do
      result <- runDeferrable$ do
         memoryRegion <- try$ userAllocator.create (RawMemoryRegion userSocketAllocatorPoolSize)
         errdefer (userAllocator.destroy memoryRegion)
         memoryResource <- createExpHeap memoryRegion
         sockets <- try$ initialize (Just memoryResource)
         mayError$ sockets.setSocketBufferAttribute (TcpInitialSize SendAndReceive  (8*1024))
         mayError$ sockets.setSocketBufferAttribute (TcpAutoTuneMax SendAndReceive (32*1024))
      case result of
         Left err => pure (Just err)
         Right () => pure Nothing
