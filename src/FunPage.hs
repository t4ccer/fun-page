module FunPage (
  -- * Sites
  Page,
  page,
  runSite,

  -- * Reactive variables
  RVar,
  Reactive,
  Handler,
  MonadReactive (readRVar, untracked),
  writeRVar,
  modifyRVar,
  bindIO,

  -- * Markup
  Html,
  newRVar,
  text,
  textR,
  element,
  div_,
  span_,
  p_,
  h1_,
  button_,
  Attribute,
  attribute,
  booleanAttribute,
  class_,
  on,
  onClick,
  hidden,
) where

import FunPage.Html (
  Attribute,
  Html,
  attribute,
  booleanAttribute,
  button_,
  class_,
  div_,
  element,
  h1_,
  hidden,
  newRVar,
  on,
  onClick,
  p_,
  span_,
  text,
  textR,
 )
import FunPage.Reactive (
  Handler,
  MonadReactive (readRVar, untracked),
  RVar,
  Reactive,
  bindIO,
  modifyRVar,
  writeRVar,
 )
import FunPage.Site (Page, page, runSite)
