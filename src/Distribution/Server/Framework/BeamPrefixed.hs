{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Inlined from @beam-transformers@ (Database.Beam.Transformers.Prefixed.Types).
--
-- TODO: switch to the @beam-transformers@ package once it is on Hackage.
-- <https://github.com/obsidiansystems/beam-transformers>
module Distribution.Server.Framework.BeamPrefixed
  ( PrefixedTable(..)
  ) where

import Data.Kind (Constraint, Type)
import Data.Typeable (Typeable)
import Database.Beam
import GHC.Generics (Generic)

data PrefixedTable prefix tbl f = PrefixedTable
  { _prefixedTable_prefix :: prefix f
  , _prefixedTable_value  :: tbl f
  } deriving (Generic)

type HasPrefixedTableConstraint (c :: Type -> Constraint) prefix tbl f =
  ( c (prefix f)
  , c (tbl f)
  )

deriving instance HasPrefixedTableConstraint Eq prefix tbl f => Eq (PrefixedTable prefix tbl f)
deriving instance HasPrefixedTableConstraint Ord prefix tbl f => Ord (PrefixedTable prefix tbl f)
deriving instance HasPrefixedTableConstraint Show prefix tbl f => Show (PrefixedTable prefix tbl f)

instance (Beamable prefix, Beamable tbl) => Beamable (PrefixedTable prefix tbl)

instance (Beamable prefix, Typeable prefix, Table tbl) => Table (PrefixedTable prefix tbl) where
  data PrimaryKey (PrefixedTable prefix tbl) f = PrefixedTableId
    { _prefixedTableId_prefix :: prefix f
    , _prefixedTableId_value  :: PrimaryKey tbl f
    } deriving (Generic)
  primaryKey v = PrefixedTableId
    { _prefixedTableId_prefix = _prefixedTable_prefix v
    , _prefixedTableId_value  = primaryKey $ _prefixedTable_value v
    }

type HasPrefixedTableIdConstraint (c :: Type -> Constraint) prefix tbl f =
  ( c (prefix f)
  , c (PrimaryKey tbl f)
  )

deriving instance HasPrefixedTableIdConstraint Eq prefix tbl f => Eq (PrimaryKey (PrefixedTable prefix tbl) f)
deriving instance HasPrefixedTableIdConstraint Ord prefix tbl f => Ord (PrimaryKey (PrefixedTable prefix tbl) f)
deriving instance HasPrefixedTableIdConstraint Show prefix tbl f => Show (PrimaryKey (PrefixedTable prefix tbl) f)

instance (Beamable prefix, Beamable (PrimaryKey tbl)) => Beamable (PrimaryKey (PrefixedTable prefix tbl))
