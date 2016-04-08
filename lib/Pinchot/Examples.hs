-- | Examples for Pinchot are in this hierarchy.  Start out with
-- "Pinchot.Examples.Postal", which contains a sample grammar.
-- Next, "Pinchot.Examples.SyntaxTrees" shows you how to convert
-- your grammar to data types, and "Pinchot.Examples.AllRulesRecord"
-- shows how to make a product type holding an Earley
-- 'Text.Earley.Prod' for every 'Rule' in your grammar.  Then,
-- "Pinchot.Earley" shows how to generate the Earley
-- 'Text.Earley.Grammar' you need to actually parse strings.
--
-- "Pinchot.Examples.Terminalize" shows you how to generate data
-- types that will reduce any 'Rule' to the sequence of terminal
-- tokens from which it came.  This can be useful not only for
-- reconstructing the source text, but also for determining where in
-- the source text a production was found.
--
-- "Pinchot.Examples.RulesToOptics" shows how to generate lenses and
-- isos, which are valuable for navigating and manipulating large trees.

module Pinchot.Examples where
