{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyCase #-}

module Network.Protocol.Shop.Type where

import           Network.TypedProtocol.Core

data BasketState where
     EmptyBasket    :: BasketState
     NonEmptyBasket :: BasketState

data Shop item card where
     Collecting    :: BasketState -> Shop item card
     CardSelected  :: Shop item card
     CardConfirmed :: Shop item card
     OrderPlaced   :: Shop item card

instance Protocol (Shop item card) where

    data Message (Shop item card) from to where
         SelectFirst :: item -> Message (Shop item card) ('Collecting 'EmptyBasket) ('Collecting 'NonEmptyBasket)
         Select      :: item -> Message (Shop item card) ('Collecting 'NonEmptyBasket) ('Collecting 'NonEmptyBasket)
         SelectCard  :: card -> Message (Shop item card) ('Collecting 'NonEmptyBasket) 'CardSelected
         Confirm     :: Message (Shop item card) 'CardSelected 'CardConfirmed
         PlaceOrder  :: Message (Shop item card) 'CardConfirmed 'OrderPlaced
         Cancel      :: forall (any :: Shop item card). Message (Shop item card) any ('Collecting 'EmptyBasket)

    data ClientHasAgency st where
         TokEmptyBaske     :: ClientHasAgency ('Collecting 'EmptyBasket)
         TokNonEmptyBasket :: ClientHasAgency ('Collecting 'NonEmptyBasket)
         
