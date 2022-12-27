{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
import           Criterion.Main
import           Data.Functor.Identity
import qualified Data.Generic.HKD as Hig
import           Data.Generics.Internal.VL
import           GHC.Generics
import           HiFi

main :: IO ()
main = defaultMain
  [ bgroup "large record"
    [ bench "fromRecord (hi-fi)" $ whnf fromRecordHiFi testRec
    , bench "fromRecord (higgledy)" $ whnf fromRecordHiggledy testRec
    , bench "toRecord (hi-fi)" $ whnf toRecordHiFi (fromRecord testRec)
    , bench "toRecord (higgledy)" $ whnf toRecordHiggledy (Hig.deconstruct testRec)
    , bench "instantiate (hi-fi)" $ whnf instantiateHiFi 1
    , bench "instantiate (higgledy)" $ whnf instantiateHiggledy 1
    , bench "getter (hi-fi)" $ whnf (\x -> x.t200) (fromRecord testRec)
    , bench "getter (higgledy)" $ whnf (\x -> x ^. Hig.field @"t200") (fromRecordHiggledy testRec)
    , bench "setter (hi-fi)" $ whnf (setField @"t200" 5) (fromRecord testRec)
    , bench "setter (higgledy)" $ whnf (Hig.field @"t200" .~ 5) (fromRecordHiggledy testRec)
    , bench "map effect (hi-fi)" $ whnf (mapEffect (Just . runIdentity)) (fromRecord testRec)
    , bench "map effect (higgledy)" $ whnf (Hig.bmap (Just . runIdentity)) (fromRecordHiggledy testRec)
    ]
  ]

fromRecordHiFi :: Test -> HKD Test Identity
fromRecordHiFi = fromRecord

fromRecordHiggledy :: Test -> Hig.HKD Test Identity
fromRecordHiggledy = Hig.deconstruct

toRecordHiFi :: HKD Test Identity -> Test
toRecordHiFi = toRecord

toRecordHiggledy :: Hig.HKD Test Identity -> Test
toRecordHiggledy = runIdentity . Hig.construct

instantiateHiFi :: Int -> HKD Test Identity
instantiateHiFi !x = mkHKD
  { t1 = Identity x
  , t2 = Identity x
  , t3 = Identity x
  , t4 = Identity x
  , t5 = Identity x
  , t6 = Identity x
  , t7 = Identity x
  , t8 = Identity x
  , t9 = Identity x
  , t10 = Identity x
  , t11 = Identity x
  , t12 = Identity x
  , t13 = Identity x
  , t14 = Identity x
  , t15 = Identity x
  , t16 = Identity x
  , t17 = Identity x
  , t18 = Identity x
  , t19 = Identity x
  , t20 = Identity x
  , t21 = Identity x
  , t22 = Identity x
  , t23 = Identity x
  , t24 = Identity x
  , t25 = Identity x
  , t26 = Identity x
  , t27 = Identity x
  , t28 = Identity x
  , t29 = Identity x
  , t30 = Identity x
  , t31 = Identity x
  , t32 = Identity x
  , t33 = Identity x
  , t34 = Identity x
  , t35 = Identity x
  , t36 = Identity x
  , t37 = Identity x
  , t38 = Identity x
  , t39 = Identity x
  , t40 = Identity x
  , t41 = Identity x
  , t42 = Identity x
  , t43 = Identity x
  , t44 = Identity x
  , t45 = Identity x
  , t46 = Identity x
  , t47 = Identity x
  , t48 = Identity x
  , t49 = Identity x
  , t50 = Identity x
  , t51 = Identity x
  , t52 = Identity x
  , t53 = Identity x
  , t54 = Identity x
  , t55 = Identity x
  , t56 = Identity x
  , t57 = Identity x
  , t58 = Identity x
  , t59 = Identity x
  , t60 = Identity x
  , t61 = Identity x
  , t62 = Identity x
  , t63 = Identity x
  , t64 = Identity x
  , t65 = Identity x
  , t66 = Identity x
  , t67 = Identity x
  , t68 = Identity x
  , t69 = Identity x
  , t70 = Identity x
  , t71 = Identity x
  , t72 = Identity x
  , t73 = Identity x
  , t74 = Identity x
  , t75 = Identity x
  , t76 = Identity x
  , t77 = Identity x
  , t78 = Identity x
  , t79 = Identity x
  , t80 = Identity x
  , t81 = Identity x
  , t82 = Identity x
  , t83 = Identity x
  , t84 = Identity x
  , t85 = Identity x
  , t86 = Identity x
  , t87 = Identity x
  , t88 = Identity x
  , t89 = Identity x
  , t90 = Identity x
  , t91 = Identity x
  , t92 = Identity x
  , t93 = Identity x
  , t94 = Identity x
  , t95 = Identity x
  , t96 = Identity x
  , t97 = Identity x
  , t98 = Identity x
  , t99 = Identity x
  , t100 = Identity x
  , t101 = Identity x
  , t102 = Identity x
  , t103 = Identity x
  , t104 = Identity x
  , t105 = Identity x
  , t106 = Identity x
  , t107 = Identity x
  , t108 = Identity x
  , t109 = Identity x
  , t110 = Identity x
  , t111 = Identity x
  , t112 = Identity x
  , t113 = Identity x
  , t114 = Identity x
  , t115 = Identity x
  , t116 = Identity x
  , t117 = Identity x
  , t118 = Identity x
  , t119 = Identity x
  , t120 = Identity x
  , t121 = Identity x
  , t122 = Identity x
  , t123 = Identity x
  , t124 = Identity x
  , t125 = Identity x
  , t126 = Identity x
  , t127 = Identity x
  , t128 = Identity x
  , t129 = Identity x
  , t130 = Identity x
  , t131 = Identity x
  , t132 = Identity x
  , t133 = Identity x
  , t134 = Identity x
  , t135 = Identity x
  , t136 = Identity x
  , t137 = Identity x
  , t138 = Identity x
  , t139 = Identity x
  , t140 = Identity x
  , t141 = Identity x
  , t142 = Identity x
  , t143 = Identity x
  , t144 = Identity x
  , t145 = Identity x
  , t146 = Identity x
  , t147 = Identity x
  , t148 = Identity x
  , t149 = Identity x
  , t150 = Identity x
  , t151 = Identity x
  , t152 = Identity x
  , t153 = Identity x
  , t154 = Identity x
  , t155 = Identity x
  , t156 = Identity x
  , t157 = Identity x
  , t158 = Identity x
  , t159 = Identity x
  , t160 = Identity x
  , t161 = Identity x
  , t162 = Identity x
  , t163 = Identity x
  , t164 = Identity x
  , t165 = Identity x
  , t166 = Identity x
  , t167 = Identity x
  , t168 = Identity x
  , t169 = Identity x
  , t170 = Identity x
  , t171 = Identity x
  , t172 = Identity x
  , t173 = Identity x
  , t174 = Identity x
  , t175 = Identity x
  , t176 = Identity x
  , t177 = Identity x
  , t178 = Identity x
  , t179 = Identity x
  , t180 = Identity x
  , t181 = Identity x
  , t182 = Identity x
  , t183 = Identity x
  , t184 = Identity x
  , t185 = Identity x
  , t186 = Identity x
  , t187 = Identity x
  , t188 = Identity x
  , t189 = Identity x
  , t190 = Identity x
  , t191 = Identity x
  , t192 = Identity x
  , t193 = Identity x
  , t194 = Identity x
  , t195 = Identity x
  , t196 = Identity x
  , t197 = Identity x
  , t198 = Identity x
  , t199 = Identity x
  , t200 = Identity x
  , t201 = Identity x
  , t202 = Identity x
  , t203 = Identity x
  , t204 = Identity x
  , t205 = Identity x
  , t206 = Identity x
  , t207 = Identity x
  , t208 = Identity x
  , t209 = Identity x
  , t210 = Identity x
  , t211 = Identity x
  , t212 = Identity x
  , t213 = Identity x
  , t214 = Identity x
  , t215 = Identity x
  , t216 = Identity x
  , t217 = Identity x
  , t218 = Identity x
  , t219 = Identity x
  , t220 = Identity x
  , t221 = Identity x
  , t222 = Identity x
  , t223 = Identity x
  , t224 = Identity x
  , t225 = Identity x
  , t226 = Identity x
  , t227 = Identity x
  , t228 = Identity x
  , t229 = Identity x
  , t230 = Identity x
  , t231 = Identity x
  , t232 = Identity x
  , t233 = Identity x
  , t234 = Identity x
  , t235 = Identity x
  , t236 = Identity x
  , t237 = Identity x
  , t238 = Identity x
  , t239 = Identity x
  , t240 = Identity x
  , t241 = Identity x
  , t242 = Identity x
  , t243 = Identity x
  , t244 = Identity x
  , t245 = Identity x
  , t246 = Identity x
  , t247 = Identity x
  , t248 = Identity x
  , t249 = Identity x
  , t250 = Identity x
  , t251 = Identity x
  , t252 = Identity x
  , t253 = Identity x
  , t254 = Identity x
  , t255 = Identity x
  , t256 = Identity x
  , t257 = Identity x
  , t258 = Identity x
  , t259 = Identity x
  , t260 = Identity x
  , t261 = Identity x
  , t262 = Identity x
  , t263 = Identity x
  , t264 = Identity x
  , t265 = Identity x
  , t266 = Identity x
  , t267 = Identity x
  , t268 = Identity x
  , t269 = Identity x
  , t270 = Identity x
  , t271 = Identity x
  , t272 = Identity x
  , t273 = Identity x
  , t274 = Identity x
  , t275 = Identity x
  , t276 = Identity x
  , t277 = Identity x
  , t278 = Identity x
  , t279 = Identity x
  , t280 = Identity x
  , t281 = Identity x
  , t282 = Identity x
  , t283 = Identity x
  , t284 = Identity x
  , t285 = Identity x
  , t286 = Identity x
  , t287 = Identity x
  , t288 = Identity x
  , t289 = Identity x
  , t290 = Identity x
  , t291 = Identity x
  , t292 = Identity x
  , t293 = Identity x
  , t294 = Identity x
  , t295 = Identity x
  , t296 = Identity x
  , t297 = Identity x
  , t298 = Identity x
  , t299 = Identity x
  , t300 = Identity x
  , t301 = Identity x
  , t302 = Identity x
  , t303 = Identity x
  , t304 = Identity x
  , t305 = Identity x
  , t306 = Identity x
  , t307 = Identity x
  , t308 = Identity x
  , t309 = Identity x
  , t310 = Identity x
  , t311 = Identity x
  , t312 = Identity x
  , t313 = Identity x
  , t314 = Identity x
  , t315 = Identity x
  , t316 = Identity x
  , t317 = Identity x
  , t318 = Identity x
  , t319 = Identity x
  , t320 = Identity x
  , t321 = Identity x
  , t322 = Identity x
  , t323 = Identity x
  , t324 = Identity x
  , t325 = Identity x
  , t326 = Identity x
  , t327 = Identity x
  , t328 = Identity x
  , t329 = Identity x
  , t330 = Identity x
  , t331 = Identity x
  , t332 = Identity x
  , t333 = Identity x
  , t334 = Identity x
  , t335 = Identity x
  , t336 = Identity x
  , t337 = Identity x
  , t338 = Identity x
  , t339 = Identity x
  , t340 = Identity x
  , t341 = Identity x
  , t342 = Identity x
  , t343 = Identity x
  , t344 = Identity x
  , t345 = Identity x
  , t346 = Identity x
  , t347 = Identity x
  , t348 = Identity x
  , t349 = Identity x
  , t350 = Identity x
  , t351 = Identity x
  , t352 = Identity x
  , t353 = Identity x
  , t354 = Identity x
  , t355 = Identity x
  , t356 = Identity x
  , t357 = Identity x
  , t358 = Identity x
  , t359 = Identity x
  , t360 = Identity x
  , t361 = Identity x
  , t362 = Identity x
  , t363 = Identity x
  , t364 = Identity x
  , t365 = Identity x
  , t366 = Identity x
  , t367 = Identity x
  , t368 = Identity x
  , t369 = Identity x
  , t370 = Identity x
  , t371 = Identity x
  , t372 = Identity x
  , t373 = Identity x
  , t374 = Identity x
  , t375 = Identity x
  , t376 = Identity x
  , t377 = Identity x
  , t378 = Identity x
  , t379 = Identity x
  , t380 = Identity x
  , t381 = Identity x
  , t382 = Identity x
  , t383 = Identity x
  , t384 = Identity x
  , t385 = Identity x
  , t386 = Identity x
  , t387 = Identity x
  , t388 = Identity x
  , t389 = Identity x
  , t390 = Identity x
  , t391 = Identity x
  , t392 = Identity x
  , t393 = Identity x
  , t394 = Identity x
  , t395 = Identity x
  , t396 = Identity x
  , t397 = Identity x
  , t398 = Identity x
  , t399 = Identity x
  , t400 = Identity x
  }

instantiateHiggledy :: Int -> Hig.HKD Test Identity
instantiateHiggledy !x =
  Hig.build @Test
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)
    (Identity x)

testRec :: Test
testRec = Test
  { t1 = 123
  , t2 = 123
  , t3 = 123
  , t4 = 123
  , t5 = 123
  , t6 = 123
  , t7 = 123
  , t8 = 123
  , t9 = 123
  , t10 = 123
  , t11 = 123
  , t12 = 123
  , t13 = 123
  , t14 = 123
  , t15 = 123
  , t16 = 123
  , t17 = 123
  , t18 = 123
  , t19 = 123
  , t20 = 123
  , t21 = 123
  , t22 = 123
  , t23 = 123
  , t24 = 123
  , t25 = 123
  , t26 = 123
  , t27 = 123
  , t28 = 123
  , t29 = 123
  , t30 = 123
  , t31 = 123
  , t32 = 123
  , t33 = 123
  , t34 = 123
  , t35 = 123
  , t36 = 123
  , t37 = 123
  , t38 = 123
  , t39 = 123
  , t40 = 123
  , t41 = 123
  , t42 = 123
  , t43 = 123
  , t44 = 123
  , t45 = 123
  , t46 = 123
  , t47 = 123
  , t48 = 123
  , t49 = 123
  , t50 = 123
  , t51 = 123
  , t52 = 123
  , t53 = 123
  , t54 = 123
  , t55 = 123
  , t56 = 123
  , t57 = 123
  , t58 = 123
  , t59 = 123
  , t60 = 123
  , t61 = 123
  , t62 = 123
  , t63 = 123
  , t64 = 123
  , t65 = 123
  , t66 = 123
  , t67 = 123
  , t68 = 123
  , t69 = 123
  , t70 = 123
  , t71 = 123
  , t72 = 123
  , t73 = 123
  , t74 = 123
  , t75 = 123
  , t76 = 123
  , t77 = 123
  , t78 = 123
  , t79 = 123
  , t80 = 123
  , t81 = 123
  , t82 = 123
  , t83 = 123
  , t84 = 123
  , t85 = 123
  , t86 = 123
  , t87 = 123
  , t88 = 123
  , t89 = 123
  , t90 = 123
  , t91 = 123
  , t92 = 123
  , t93 = 123
  , t94 = 123
  , t95 = 123
  , t96 = 123
  , t97 = 123
  , t98 = 123
  , t99 = 123
  , t100 = 123
  , t101 = 123
  , t102 = 123
  , t103 = 123
  , t104 = 123
  , t105 = 123
  , t106 = 123
  , t107 = 123
  , t108 = 123
  , t109 = 123
  , t110 = 123
  , t111 = 123
  , t112 = 123
  , t113 = 123
  , t114 = 123
  , t115 = 123
  , t116 = 123
  , t117 = 123
  , t118 = 123
  , t119 = 123
  , t120 = 123
  , t121 = 123
  , t122 = 123
  , t123 = 123
  , t124 = 123
  , t125 = 123
  , t126 = 123
  , t127 = 123
  , t128 = 123
  , t129 = 123
  , t130 = 123
  , t131 = 123
  , t132 = 123
  , t133 = 123
  , t134 = 123
  , t135 = 123
  , t136 = 123
  , t137 = 123
  , t138 = 123
  , t139 = 123
  , t140 = 123
  , t141 = 123
  , t142 = 123
  , t143 = 123
  , t144 = 123
  , t145 = 123
  , t146 = 123
  , t147 = 123
  , t148 = 123
  , t149 = 123
  , t150 = 123
  , t151 = 123
  , t152 = 123
  , t153 = 123
  , t154 = 123
  , t155 = 123
  , t156 = 123
  , t157 = 123
  , t158 = 123
  , t159 = 123
  , t160 = 123
  , t161 = 123
  , t162 = 123
  , t163 = 123
  , t164 = 123
  , t165 = 123
  , t166 = 123
  , t167 = 123
  , t168 = 123
  , t169 = 123
  , t170 = 123
  , t171 = 123
  , t172 = 123
  , t173 = 123
  , t174 = 123
  , t175 = 123
  , t176 = 123
  , t177 = 123
  , t178 = 123
  , t179 = 123
  , t180 = 123
  , t181 = 123
  , t182 = 123
  , t183 = 123
  , t184 = 123
  , t185 = 123
  , t186 = 123
  , t187 = 123
  , t188 = 123
  , t189 = 123
  , t190 = 123
  , t191 = 123
  , t192 = 123
  , t193 = 123
  , t194 = 123
  , t195 = 123
  , t196 = 123
  , t197 = 123
  , t198 = 123
  , t199 = 123
  , t200 = 123
  , t201 = 123
  , t202 = 123
  , t203 = 123
  , t204 = 123
  , t205 = 123
  , t206 = 123
  , t207 = 123
  , t208 = 123
  , t209 = 123
  , t210 = 123
  , t211 = 123
  , t212 = 123
  , t213 = 123
  , t214 = 123
  , t215 = 123
  , t216 = 123
  , t217 = 123
  , t218 = 123
  , t219 = 123
  , t220 = 123
  , t221 = 123
  , t222 = 123
  , t223 = 123
  , t224 = 123
  , t225 = 123
  , t226 = 123
  , t227 = 123
  , t228 = 123
  , t229 = 123
  , t230 = 123
  , t231 = 123
  , t232 = 123
  , t233 = 123
  , t234 = 123
  , t235 = 123
  , t236 = 123
  , t237 = 123
  , t238 = 123
  , t239 = 123
  , t240 = 123
  , t241 = 123
  , t242 = 123
  , t243 = 123
  , t244 = 123
  , t245 = 123
  , t246 = 123
  , t247 = 123
  , t248 = 123
  , t249 = 123
  , t250 = 123
  , t251 = 123
  , t252 = 123
  , t253 = 123
  , t254 = 123
  , t255 = 123
  , t256 = 123
  , t257 = 123
  , t258 = 123
  , t259 = 123
  , t260 = 123
  , t261 = 123
  , t262 = 123
  , t263 = 123
  , t264 = 123
  , t265 = 123
  , t266 = 123
  , t267 = 123
  , t268 = 123
  , t269 = 123
  , t270 = 123
  , t271 = 123
  , t272 = 123
  , t273 = 123
  , t274 = 123
  , t275 = 123
  , t276 = 123
  , t277 = 123
  , t278 = 123
  , t279 = 123
  , t280 = 123
  , t281 = 123
  , t282 = 123
  , t283 = 123
  , t284 = 123
  , t285 = 123
  , t286 = 123
  , t287 = 123
  , t288 = 123
  , t289 = 123
  , t290 = 123
  , t291 = 123
  , t292 = 123
  , t293 = 123
  , t294 = 123
  , t295 = 123
  , t296 = 123
  , t297 = 123
  , t298 = 123
  , t299 = 123
  , t300 = 123
  , t301 = 123
  , t302 = 123
  , t303 = 123
  , t304 = 123
  , t305 = 123
  , t306 = 123
  , t307 = 123
  , t308 = 123
  , t309 = 123
  , t310 = 123
  , t311 = 123
  , t312 = 123
  , t313 = 123
  , t314 = 123
  , t315 = 123
  , t316 = 123
  , t317 = 123
  , t318 = 123
  , t319 = 123
  , t320 = 123
  , t321 = 123
  , t322 = 123
  , t323 = 123
  , t324 = 123
  , t325 = 123
  , t326 = 123
  , t327 = 123
  , t328 = 123
  , t329 = 123
  , t330 = 123
  , t331 = 123
  , t332 = 123
  , t333 = 123
  , t334 = 123
  , t335 = 123
  , t336 = 123
  , t337 = 123
  , t338 = 123
  , t339 = 123
  , t340 = 123
  , t341 = 123
  , t342 = 123
  , t343 = 123
  , t344 = 123
  , t345 = 123
  , t346 = 123
  , t347 = 123
  , t348 = 123
  , t349 = 123
  , t350 = 123
  , t351 = 123
  , t352 = 123
  , t353 = 123
  , t354 = 123
  , t355 = 123
  , t356 = 123
  , t357 = 123
  , t358 = 123
  , t359 = 123
  , t360 = 123
  , t361 = 123
  , t362 = 123
  , t363 = 123
  , t364 = 123
  , t365 = 123
  , t366 = 123
  , t367 = 123
  , t368 = 123
  , t369 = 123
  , t370 = 123
  , t371 = 123
  , t372 = 123
  , t373 = 123
  , t374 = 123
  , t375 = 123
  , t376 = 123
  , t377 = 123
  , t378 = 123
  , t379 = 123
  , t380 = 123
  , t381 = 123
  , t382 = 123
  , t383 = 123
  , t384 = 123
  , t385 = 123
  , t386 = 123
  , t387 = 123
  , t388 = 123
  , t389 = 123
  , t390 = 123
  , t391 = 123
  , t392 = 123
  , t393 = 123
  , t394 = 123
  , t395 = 123
  , t396 = 123
  , t397 = 123
  , t398 = 123
  , t399 = 123
  , t400 = 123
  }

data Test = Test
  { t1 :: Int
  , t2 :: Int
  , t3 :: Int
  , t4 :: Int
  , t5 :: Int
  , t6 :: Int
  , t7 :: Int
  , t8 :: Int
  , t9 :: Int
  , t10 :: Int
  , t11 :: Int
  , t12 :: Int
  , t13 :: Int
  , t14 :: Int
  , t15 :: Int
  , t16 :: Int
  , t17 :: Int
  , t18 :: Int
  , t19 :: Int
  , t20 :: Int
  , t21 :: Int
  , t22 :: Int
  , t23 :: Int
  , t24 :: Int
  , t25 :: Int
  , t26 :: Int
  , t27 :: Int
  , t28 :: Int
  , t29 :: Int
  , t30 :: Int
  , t31 :: Int
  , t32 :: Int
  , t33 :: Int
  , t34 :: Int
  , t35 :: Int
  , t36 :: Int
  , t37 :: Int
  , t38 :: Int
  , t39 :: Int
  , t40 :: Int
  , t41 :: Int
  , t42 :: Int
  , t43 :: Int
  , t44 :: Int
  , t45 :: Int
  , t46 :: Int
  , t47 :: Int
  , t48 :: Int
  , t49 :: Int
  , t50 :: Int
  , t51 :: Int
  , t52 :: Int
  , t53 :: Int
  , t54 :: Int
  , t55 :: Int
  , t56 :: Int
  , t57 :: Int
  , t58 :: Int
  , t59 :: Int
  , t60 :: Int
  , t61 :: Int
  , t62 :: Int
  , t63 :: Int
  , t64 :: Int
  , t65 :: Int
  , t66 :: Int
  , t67 :: Int
  , t68 :: Int
  , t69 :: Int
  , t70 :: Int
  , t71 :: Int
  , t72 :: Int
  , t73 :: Int
  , t74 :: Int
  , t75 :: Int
  , t76 :: Int
  , t77 :: Int
  , t78 :: Int
  , t79 :: Int
  , t80 :: Int
  , t81 :: Int
  , t82 :: Int
  , t83 :: Int
  , t84 :: Int
  , t85 :: Int
  , t86 :: Int
  , t87 :: Int
  , t88 :: Int
  , t89 :: Int
  , t90 :: Int
  , t91 :: Int
  , t92 :: Int
  , t93 :: Int
  , t94 :: Int
  , t95 :: Int
  , t96 :: Int
  , t97 :: Int
  , t98 :: Int
  , t99 :: Int
  , t100 :: Int
  , t101 :: Int
  , t102 :: Int
  , t103 :: Int
  , t104 :: Int
  , t105 :: Int
  , t106 :: Int
  , t107 :: Int
  , t108 :: Int
  , t109 :: Int
  , t110 :: Int
  , t111 :: Int
  , t112 :: Int
  , t113 :: Int
  , t114 :: Int
  , t115 :: Int
  , t116 :: Int
  , t117 :: Int
  , t118 :: Int
  , t119 :: Int
  , t120 :: Int
  , t121 :: Int
  , t122 :: Int
  , t123 :: Int
  , t124 :: Int
  , t125 :: Int
  , t126 :: Int
  , t127 :: Int
  , t128 :: Int
  , t129 :: Int
  , t130 :: Int
  , t131 :: Int
  , t132 :: Int
  , t133 :: Int
  , t134 :: Int
  , t135 :: Int
  , t136 :: Int
  , t137 :: Int
  , t138 :: Int
  , t139 :: Int
  , t140 :: Int
  , t141 :: Int
  , t142 :: Int
  , t143 :: Int
  , t144 :: Int
  , t145 :: Int
  , t146 :: Int
  , t147 :: Int
  , t148 :: Int
  , t149 :: Int
  , t150 :: Int
  , t151 :: Int
  , t152 :: Int
  , t153 :: Int
  , t154 :: Int
  , t155 :: Int
  , t156 :: Int
  , t157 :: Int
  , t158 :: Int
  , t159 :: Int
  , t160 :: Int
  , t161 :: Int
  , t162 :: Int
  , t163 :: Int
  , t164 :: Int
  , t165 :: Int
  , t166 :: Int
  , t167 :: Int
  , t168 :: Int
  , t169 :: Int
  , t170 :: Int
  , t171 :: Int
  , t172 :: Int
  , t173 :: Int
  , t174 :: Int
  , t175 :: Int
  , t176 :: Int
  , t177 :: Int
  , t178 :: Int
  , t179 :: Int
  , t180 :: Int
  , t181 :: Int
  , t182 :: Int
  , t183 :: Int
  , t184 :: Int
  , t185 :: Int
  , t186 :: Int
  , t187 :: Int
  , t188 :: Int
  , t189 :: Int
  , t190 :: Int
  , t191 :: Int
  , t192 :: Int
  , t193 :: Int
  , t194 :: Int
  , t195 :: Int
  , t196 :: Int
  , t197 :: Int
  , t198 :: Int
  , t199 :: Int
  , t200 :: Int
  , t201 :: Int
  , t202 :: Int
  , t203 :: Int
  , t204 :: Int
  , t205 :: Int
  , t206 :: Int
  , t207 :: Int
  , t208 :: Int
  , t209 :: Int
  , t210 :: Int
  , t211 :: Int
  , t212 :: Int
  , t213 :: Int
  , t214 :: Int
  , t215 :: Int
  , t216 :: Int
  , t217 :: Int
  , t218 :: Int
  , t219 :: Int
  , t220 :: Int
  , t221 :: Int
  , t222 :: Int
  , t223 :: Int
  , t224 :: Int
  , t225 :: Int
  , t226 :: Int
  , t227 :: Int
  , t228 :: Int
  , t229 :: Int
  , t230 :: Int
  , t231 :: Int
  , t232 :: Int
  , t233 :: Int
  , t234 :: Int
  , t235 :: Int
  , t236 :: Int
  , t237 :: Int
  , t238 :: Int
  , t239 :: Int
  , t240 :: Int
  , t241 :: Int
  , t242 :: Int
  , t243 :: Int
  , t244 :: Int
  , t245 :: Int
  , t246 :: Int
  , t247 :: Int
  , t248 :: Int
  , t249 :: Int
  , t250 :: Int
  , t251 :: Int
  , t252 :: Int
  , t253 :: Int
  , t254 :: Int
  , t255 :: Int
  , t256 :: Int
  , t257 :: Int
  , t258 :: Int
  , t259 :: Int
  , t260 :: Int
  , t261 :: Int
  , t262 :: Int
  , t263 :: Int
  , t264 :: Int
  , t265 :: Int
  , t266 :: Int
  , t267 :: Int
  , t268 :: Int
  , t269 :: Int
  , t270 :: Int
  , t271 :: Int
  , t272 :: Int
  , t273 :: Int
  , t274 :: Int
  , t275 :: Int
  , t276 :: Int
  , t277 :: Int
  , t278 :: Int
  , t279 :: Int
  , t280 :: Int
  , t281 :: Int
  , t282 :: Int
  , t283 :: Int
  , t284 :: Int
  , t285 :: Int
  , t286 :: Int
  , t287 :: Int
  , t288 :: Int
  , t289 :: Int
  , t290 :: Int
  , t291 :: Int
  , t292 :: Int
  , t293 :: Int
  , t294 :: Int
  , t295 :: Int
  , t296 :: Int
  , t297 :: Int
  , t298 :: Int
  , t299 :: Int
  , t300 :: Int
  , t301 :: Int
  , t302 :: Int
  , t303 :: Int
  , t304 :: Int
  , t305 :: Int
  , t306 :: Int
  , t307 :: Int
  , t308 :: Int
  , t309 :: Int
  , t310 :: Int
  , t311 :: Int
  , t312 :: Int
  , t313 :: Int
  , t314 :: Int
  , t315 :: Int
  , t316 :: Int
  , t317 :: Int
  , t318 :: Int
  , t319 :: Int
  , t320 :: Int
  , t321 :: Int
  , t322 :: Int
  , t323 :: Int
  , t324 :: Int
  , t325 :: Int
  , t326 :: Int
  , t327 :: Int
  , t328 :: Int
  , t329 :: Int
  , t330 :: Int
  , t331 :: Int
  , t332 :: Int
  , t333 :: Int
  , t334 :: Int
  , t335 :: Int
  , t336 :: Int
  , t337 :: Int
  , t338 :: Int
  , t339 :: Int
  , t340 :: Int
  , t341 :: Int
  , t342 :: Int
  , t343 :: Int
  , t344 :: Int
  , t345 :: Int
  , t346 :: Int
  , t347 :: Int
  , t348 :: Int
  , t349 :: Int
  , t350 :: Int
  , t351 :: Int
  , t352 :: Int
  , t353 :: Int
  , t354 :: Int
  , t355 :: Int
  , t356 :: Int
  , t357 :: Int
  , t358 :: Int
  , t359 :: Int
  , t360 :: Int
  , t361 :: Int
  , t362 :: Int
  , t363 :: Int
  , t364 :: Int
  , t365 :: Int
  , t366 :: Int
  , t367 :: Int
  , t368 :: Int
  , t369 :: Int
  , t370 :: Int
  , t371 :: Int
  , t372 :: Int
  , t373 :: Int
  , t374 :: Int
  , t375 :: Int
  , t376 :: Int
  , t377 :: Int
  , t378 :: Int
  , t379 :: Int
  , t380 :: Int
  , t381 :: Int
  , t382 :: Int
  , t383 :: Int
  , t384 :: Int
  , t385 :: Int
  , t386 :: Int
  , t387 :: Int
  , t388 :: Int
  , t389 :: Int
  , t390 :: Int
  , t391 :: Int
  , t392 :: Int
  , t393 :: Int
  , t394 :: Int
  , t395 :: Int
  , t396 :: Int
  , t397 :: Int
  , t398 :: Int
  , t399 :: Int
  , t400 :: Int
  } deriving Generic
