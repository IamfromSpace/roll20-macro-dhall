let Map = https://prelude.dhall-lang.org/Map/Type.dhall

let Entry = https://prelude.dhall-lang.org/Map/Entry.dhall

let Bool/not =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/Bool/not.dhall

let Natural/enumerate =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/Natural/enumerate.dhall

let Optional/map =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/Optional/map.dhall

let List/map =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/List/map.dhall

let List/concatMap =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/List/concatMap.dhall

let Text/concatSep =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.1.0/Prelude/Text/concatSep.dhall

let foldLeft = https://prelude.dhall-lang.org/List/foldLeft.dhall

let toGM = \(t : Text) -> "/w GM ${t}"

let toTemplateStr =
      \(name : Text) ->
      \(xs : Map Text Text) ->
            "&{template:default}"
        ++  foldLeft
              (Entry Text Text)
              (toMap { name } # xs)
              Text
              ( \(t : Text) ->
                \(e : Entry Text Text) ->
                  "${t} {{${e.mapKey}=${e.mapValue}}}"
              )
              ""

let trickAttack =
      \ ( c
        : { characterName : Text
          , attackName : Text
          , trickBonus : Natural
          , trickDamage : Text
          , bonus : Natural
          , damageRoll : Text
          , addedDamage : Natural
          , damageType : Text
          , critEffect : Optional Text
          , ammo : Optional { capacity : Natural, charges : Natural }
          }
        ) ->
        let damageStr =
              "[[${c.damageRoll} + ${Natural/show
                                       c.addedDamage}]]${c.damageType} + [[${c.trickDamage}]] if tricked"

        let attackStr = "[[d20 + ${Natural/show c.bonus}]]"

        let critStr =
              merge
                { None = "", Some = \(t : Text) -> " and ${t}" }
                c.critEffect

        let critRollStr = damageStr ++ critStr

        let ammoStr =
              merge
                { None = [] : Map Text Text
                , Some =
                    \(a : { charges : Natural, capacity : Natural }) ->
                      [ { mapKey = "Ammo (${Natural/show a.charges})"
                        , mapValue = "[[d${Natural/show a.capacity}]]"
                        }
                      ]
                }
                c.ammo

        in  toTemplateStr
              "${c.characterName} - ${c.attackName} - Trick Attack"
              (   toMap
                    { Trick = "[[d20 + ${Natural/show c.trickBonus}]]"
                    , Attack = attackStr
                    , Damage = damageStr
                    , Crit = critRollStr
                    }
                # ammoStr
              )

let Target = < Selected | Named : Text | Implicit >

let SelectOption =
      \(a : Type) -> < Unlabeled : a | Labeled : { label : Text, value : a } >

let SelectOption/map =
      \(a : Type) ->
      \(b : Type) ->
      \(f : a -> b) ->
      \(x : SelectOption a) ->
        merge
          { Unlabeled = \(y : a) -> (SelectOption b).Unlabeled (f y)
          , Labeled =
              \(y : { label : Text, value : a }) ->
                (SelectOption b).Labeled { label = y.label, value = f y.value }
          }
          x

let QueryOptions =
      \(a : Type) -> < Defaulted : a | DropDown : List (SelectOption a) >

let QueryOptions/map =
      \(a : Type) ->
      \(b : Type) ->
      \(f : a -> b) ->
      \(x : QueryOptions a) ->
        merge
          { Defaulted = \(x : a) -> (QueryOptions b).Defaulted (f x)
          , DropDown =
              \(xs : List (SelectOption a)) ->
                (QueryOptions b).DropDown
                  ( List/map
                      (SelectOption a)
                      (SelectOption b)
                      (SelectOption/map a b f)
                      xs
                  )
          }
          x

let SimpleAst
    : Type -> Type
    = \(a : Type) ->
        < Value : a
        | Macro : Text
        | Ability : { char : Target, name : Text }
        | Attribute : { char : Target, name : Text }
        | Query : { id : Text, options : QueryOptions a }
        >

let SimpleAst/map =
      \(a : Type) ->
      \(b : Type) ->
      \(f : a -> b) ->
      \(x : SimpleAst a) ->
        merge
          { Value = \(y : a) -> (SimpleAst b).Value (f y)
          , Macro = \(y : Text) -> (SimpleAst b).Macro y
          , Ability =
              \(y : { char : Target, name : Text }) -> (SimpleAst b).Ability y
          , Attribute =
              \(y : { char : Target, name : Text }) -> (SimpleAst b).Attribute y
          , Query =
              \(y : { id : Text, options : QueryOptions a }) ->
                (SimpleAst b).Query
                  { id = y.id, options = QueryOptions/map a b f y.options }
          }
          x

let renderSimpleAst =
      \(a : Type) ->
      \(f : a -> Text) ->
      \(x : SimpleAst a) ->
        merge
          { Value = \(x : a) -> f x
          , Macro = \(t : Text) -> "#${t}"
          , Ability =
              \(x : { char : Target, name : Text }) ->
                let target =
                      merge
                        { Implicit = ""
                        , Selected = "selected|"
                        , Named = \(name : Text) -> "${name}|"
                        }
                        x.char

                in  "%{${target}${x.name}}"
          , Attribute =
              \(x : { char : Target, name : Text }) ->
                let target =
                      merge
                        { Implicit = ""
                        , Selected = "selected|"
                        , Named = \(name : Text) -> "${name}|"
                        }
                        x.char

                in  "@{${target}${x.name}}"
          , Query =
              \(x : { id : Text, options : QueryOptions a }) ->
                let options =
                      merge
                        { Defaulted = \(a : a) -> f a
                        , DropDown =
                            \(list : List (SelectOption a)) ->
                              Text/concatSep
                                "|"
                                ( List/map
                                    (SelectOption a)
                                    Text
                                    ( \(option : SelectOption a) ->
                                        merge
                                          { Unlabeled = \(x : a) -> f x
                                          , Labeled =
                                              \ ( x
                                                : { label : Text, value : a }
                                                ) ->
                                                "${x.label}${f x.value}"
                                          }
                                          option
                                    )
                                    list
                                )
                        }
                        x.options

                in  "?{${x.id}|${options}}"
          }
          x

let Text/id = \(x : Text) -> x

let multiAttackEntriesSimpleAst =
      \ ( x
        : { bonus : SimpleAst Natural
          , damageRoll : SimpleAst Text
          , damageType : SimpleAst Text
          , critEffect : Optional (SimpleAst Text)
          , modifier : SimpleAst Natural
          , attackCountPenalty : SimpleAst Natural
          , attackCount : Natural
          }
        ) ->
        let damageExpr = "[[${renderSimpleAst Text Text/id x.damageRoll}]]"

        let critStr =
              merge
                { None = ""
                , Some =
                    \(effect : SimpleAst Text) ->
                      " and ${renderSimpleAst Text Text/id effect}"
                }
                x.critEffect

        let critRollStr = damageExpr ++ critStr

        in  List/concatMap
              Natural
              (Entry Text Text)
              ( \(i : Natural) ->
                  let i1 = Natural/show (i + 1)

                  in  [ { mapKey = "Attack ${i1}"
                        , mapValue =
                            "[[d20 + ${renderSimpleAst
                                         Natural
                                         Natural/show
                                         x.bonus} + ${renderSimpleAst
                                                        Natural
                                                        Natural/show
                                                        x.modifier} - ${renderSimpleAst
                                                                          Natural
                                                                          Natural/show
                                                                          x.attackCountPenalty}]]"
                        }
                      , { mapKey = "Damage ${i1}"
                        , mapValue =
                                damageExpr
                            ++  renderSimpleAst Text Text/id x.damageType
                        }
                      , { mapKey = "Crit Dmg ${i1}", mapValue = critRollStr }
                      ]
              )
              (Natural/enumerate x.attackCount)

let multiAttackSimpleAstExt =
      \ ( x
        : { charName : SimpleAst Text
          , attackName : SimpleAst Text
          , bonus : SimpleAst Natural
          , attackCount : Natural
          , attackCountPenalty : SimpleAst Natural
          , damageRoll : SimpleAst Text
          , damageType : SimpleAst Text
          , critEffect : Optional (SimpleAst Text)
          , ammo :
              Optional
                { capacity : SimpleAst Natural, charges : SimpleAst Natural }
          , otherKV : Map Text Text
          }
        ) ->
        let ammoKV =
              merge
                { None = [] : Map Text Text
                , Some =
                    \ ( a
                      : { charges : SimpleAst Natural
                        , capacity : SimpleAst Natural
                        }
                      ) ->
                      [ { mapKey =
                            "Ammo ([[${Natural/show
                                         x.attackCount} * ${renderSimpleAst
                                                              Natural
                                                              Natural/show
                                                              a.charges})}]])"
                        , mapValue =
                            "[[d${renderSimpleAst
                                    Natural
                                    Natural/show
                                    a.capacity}]]"
                        }
                      ]
                }
                x.ammo

        in  toTemplateStr
              "${renderSimpleAst Text Text/id x.charName} - ${renderSimpleAst
                                                                Text
                                                                Text/id
                                                                x.attackName}"
              (   multiAttackEntriesSimpleAst
                    { bonus = x.bonus
                    , damageRoll = x.damageRoll
                    , damageType = x.damageType
                    , critEffect = x.critEffect
                    , modifier =
                        (SimpleAst Natural).Query
                          { id = "Modifier"
                          , options = (QueryOptions Natural).Defaulted 0
                          }
                    , attackCountPenalty = x.attackCountPenalty
                    , attackCount = x.attackCount
                    }
                # ammoKV
                # x.otherKV
              )

let multiAttackSimpleAst =
      \ ( x
        : { charName : SimpleAst Text
          , attackName : SimpleAst Text
          , bonus : SimpleAst Natural
          , attackCount : Natural
          , attackCountPenalty : SimpleAst Natural
          , damageRoll : SimpleAst Text
          , damageType : SimpleAst Text
          , critEffect : Optional (SimpleAst Text)
          , ammo :
              Optional
                { capacity : SimpleAst Natural, charges : SimpleAst Natural }
          }
        ) ->
        multiAttackSimpleAstExt (x // { otherKV = [] : Map Text Text })

let Weapon =
      { name : Text
      , attributeSafeName : Text
      , damageRoll : Text
      , damageType : Text
      , critEffect : Optional Text
      , ammo : Optional { capacity : Natural, charges : Natural }
      }

let multiAttackCharSheetExt =
      \(weapon : Weapon) ->
      \(attackCount : Natural) ->
      \(otherKV : Map Text Text) ->
        multiAttackSimpleAstExt
          { charName =
              (SimpleAst Text).Attribute
                { char = Target.Implicit, name = "character_name" }
          , attackName = (SimpleAst Text).Value weapon.name
          , bonus =
              (SimpleAst Natural).Attribute
                { char = Target.Implicit
                , name = "atk_bonus_${weapon.attributeSafeName}"
                }
          , attackCount
          , attackCountPenalty =
              if        Bool/not (Natural/isZero attackCount)
                    &&  Natural/isZero (Natural/subtract 1 attackCount)
              then  (SimpleAst Natural).Value 0
              else  (SimpleAst Natural).Attribute
                      { char = Target.Implicit
                      , name =
                          "atk_penalty_x${Natural/show
                                            attackCount}_${weapon.attributeSafeName}"
                      }
          , damageRoll =
              let bonusAttribute =
                    (SimpleAst Natural).Attribute
                      { char = Target.Implicit
                      , name = "atk_dmg_bonus_${weapon.attributeSafeName}"
                      }

              in  (SimpleAst Text).Value
                    "${weapon.damageRoll} + ${renderSimpleAst
                                                Natural
                                                Natural/show
                                                bonusAttribute}"
          , damageType = (SimpleAst Text).Value weapon.damageType
          , critEffect =
              Optional/map
                Text
                (SimpleAst Text)
                (SimpleAst Text).Value
                weapon.critEffect
          , ammo =
              Optional/map
                { capacity : Natural, charges : Natural }
                { capacity : SimpleAst Natural, charges : SimpleAst Natural }
                ( \(y : { capacity : Natural, charges : Natural }) ->
                    { capacity = (SimpleAst Natural).Value y.capacity
                    , charges = (SimpleAst Natural).Value y.charges
                    }
                )
                weapon.ammo
          , otherKV
          }

let multiAttack =
      \ ( x
        : { charName : Text
          , attackName : Text
          , bonus : Natural
          , attackCount : Natural
          , attackCountPenalty : Natural
          , damageRoll : Text
          , damageType : Text
          , critEffect : Optional Text
          , ammo : Optional { capacity : Natural, charges : Natural }
          }
        ) ->
        multiAttackSimpleAst
          { charName = (SimpleAst Text).Value x.charName
          , attackName = (SimpleAst Text).Value x.attackName
          , bonus = (SimpleAst Natural).Value x.bonus
          , attackCount = x.attackCount
          , attackCountPenalty = (SimpleAst Natural).Value x.attackCountPenalty
          , damageRoll = (SimpleAst Text).Value x.damageRoll
          , damageType = (SimpleAst Text).Value x.damageType
          , critEffect =
              Optional/map
                Text
                (SimpleAst Text)
                (SimpleAst Text).Value
                x.critEffect
          , ammo =
              Optional/map
                { capacity : Natural, charges : Natural }
                { capacity : SimpleAst Natural, charges : SimpleAst Natural }
                ( \(y : { capacity : Natural, charges : Natural }) ->
                    { capacity = (SimpleAst Natural).Value y.capacity
                    , charges = (SimpleAst Natural).Value y.charges
                    }
                )
                x.ammo
          }

let trickAttackSimpleAst =
    -- TODO: Never actually tested this
      \ ( x
        : { charName : SimpleAst Text
          , trickBonus : SimpleAst Integer
          , trickDamage : SimpleAst Text
          , bonus : SimpleAst Natural
          , addedDamage : SimpleAst Natural
          , weapon : Weapon
          }
        ) ->
        multiAttackSimpleAstExt
          { charName = x.charName
          , attackName =
              (SimpleAst Text).Value "${x.weapon.name} - Trick Attack"
          , bonus = x.bonus
          , attackCount = 1
          , attackCountPenalty = (SimpleAst Natural).Value 0
          , damageRoll =
              let addedDamage =
                    renderSimpleAst Natural Natural/show x.addedDamage

              in  (SimpleAst Text).Value
                    "${x.weapon.damageRoll} + ${addedDamage}"
          , damageType = (SimpleAst Text).Value x.weapon.damageType
          , critEffect =
              Optional/map
                Text
                (SimpleAst Text)
                (SimpleAst Text).Value
                x.weapon.critEffect
          , ammo =
              Optional/map
                { capacity : Natural, charges : Natural }
                { capacity : SimpleAst Natural, charges : SimpleAst Natural }
                ( \(y : { capacity : Natural, charges : Natural }) ->
                    { capacity = (SimpleAst Natural).Value y.capacity
                    , charges = (SimpleAst Natural).Value y.charges
                    }
                )
                x.weapon.ammo
          , otherKV =
            [ { mapKey = "Trick Check"
              , mapValue =
                  "[[d20 + ${renderSimpleAst
                               Integer
                               Integer/show
                               x.trickBonus}]]"
              }
            , { mapKey = "Trick Damage"
              , mapValue = "[[${renderSimpleAst Text Text/id x.trickDamage}]]"
              }
            ]
          }

let singleAttack =
      \ ( x
        : { charName : Text
          , attackName : Text
          , bonus : Natural
          , damageRoll : Text
          , damageType : Text
          , critEffect : Optional Text
          , ammo : Optional { capacity : Natural, charges : Natural }
          }
        ) ->
        multiAttack (x /\ { attackCount = 1, attackCountPenalty = 0 })

let doubleAttack =
      \ ( x
        : { charName : Text
          , attackName : Text
          , bonus : Natural
          , fullPenalty : Natural
          , damageRoll : Text
          , damageType : Text
          , critEffect : Optional Text
          , ammo : Optional { capacity : Natural, charges : Natural }
          }
        ) ->
        multiAttack
          { charName = x.charName
          , attackName = x.attackName
          , bonus = x.bonus
          , attackCount = 2
          , attackCountPenalty = x.fullPenalty
          , damageRoll = x.damageRoll
          , damageType = x.damageType
          , critEffect = x.critEffect
          , ammo = x.ammo
          }

let checks =
      \ ( x
        : { charName : Text
          , fort : Integer
          , ref : Integer
          , will : Integer
          , init : Integer
          }
        ) ->
        let fort = Integer/show x.fort

        let ref = Integer/show x.ref

        let will = Integer/show x.will

        let init = Integer/show x.init

        in  "${x.charName}: [[d20 ?{Check|FORT, ${fort}[FORT]|REF, ${ref}[REF]|WILL, ${will}[WILL]|Init, ${init}[Init]} + ?{Modifier|0}]]"

let basicAttacksAndChecksWithWeapon =
      let MultiOption = { attackCount : Natural, attackCountPenalty : Natural }

      let Attack =
            { weapon : Weapon
            , bonus : Natural
            , damageBonus : Natural
            , multiOptions : List MultiOption
            }

      in  \ ( x
            : { charName : Text
              , fort : Integer
              , ref : Integer
              , will : Integer
              , init : Integer
              , attacks : List Attack
              }
            ) ->
            let attacksTxt =
                  Text/concatSep
                    "\n"
                    ( List/concatMap
                        Attack
                        Text
                        ( \(a : Attack) ->
                            List/map
                              MultiOption
                              Text
                              ( \(mo : MultiOption) ->
                                  toGM
                                    ( multiAttack
                                        { charName = x.charName
                                        , attackName =
                                            "${a.weapon.name} x${Natural/show
                                                                   mo.attackCount}"
                                        , bonus = a.bonus
                                        , attackCount = mo.attackCount
                                        , attackCountPenalty =
                                            mo.attackCountPenalty
                                        , damageRoll =
                                            "${a.weapon.damageRoll} + ${Natural/show
                                                                          a.damageBonus}"
                                        , damageType = a.weapon.damageType
                                        , critEffect = a.weapon.critEffect
                                        , ammo = a.weapon.ammo
                                        }
                                    )
                              )
                              (   [ { attackCount = 1, attackCountPenalty = 0 }
                                  ]
                                # a.multiOptions
                              )
                        )
                        x.attacks
                    )

            let checksTxt =
                  toGM
                    ( checks
                        { charName = x.charName
                        , fort = x.fort
                        , ref = x.ref
                        , will = x.will
                        , init = x.init
                        }
                    )

            in  ''
                Checks:
                ${checksTxt}

                Attacks:
                ${attacksTxt}
                ''

let basicAttacksAndChecks =
      let MultiOption = { attackCount : Natural, attackCountPenalty : Natural }

      let Attack =
            { attackName : Text
            , bonus : Natural
            , multiOptions : List MultiOption
            , damageRoll : Text
            , damageType : Text
            , critEffect : Optional Text
            , ammo : Optional { capacity : Natural, charges : Natural }
            }

      in  \ ( x
            : { charName : Text
              , fort : Integer
              , ref : Integer
              , will : Integer
              , init : Integer
              , attacks : List Attack
              }
            ) ->
            let attacksTxt =
                  Text/concatSep
                    "\n"
                    ( List/concatMap
                        Attack
                        Text
                        ( \(a : Attack) ->
                            List/map
                              MultiOption
                              Text
                              ( \(mo : MultiOption) ->
                                  toGM
                                    ( multiAttack
                                        { charName = x.charName
                                        , attackName =
                                            "${a.attackName} x${Natural/show
                                                                  mo.attackCount}"
                                        , bonus = a.bonus
                                        , attackCount = mo.attackCount
                                        , attackCountPenalty =
                                            mo.attackCountPenalty
                                        , damageRoll = a.damageRoll
                                        , damageType = a.damageType
                                        , critEffect = a.critEffect
                                        , ammo = a.ammo
                                        }
                                    )
                              )
                              (   [ { attackCount = 1, attackCountPenalty = 0 }
                                  ]
                                # a.multiOptions
                              )
                        )
                        x.attacks
                    )

            let checksTxt =
                  toGM
                    ( checks
                        { charName = x.charName
                        , fort = x.fort
                        , ref = x.ref
                        , will = x.will
                        , init = x.init
                        }
                    )

            in  ''
                Checks:
                ${checksTxt}

                Attacks:
                ${attacksTxt}
                ''

let shirrenEyeRifle =
      { name = "Shirren-Eye Rifle, Advanced"
      , attributeSafeName = "shirren-eye-rifle-advanced"
      , damageRoll = "2d10"
      , damageType = "P"
      , critEffect = None Text
      , ammo = Some { capacity = 8, charges = 1 }
      }

let echoShirrenEyeRifle =
      multiAttackCharSheetExt
        shirrenEyeRifle
        1
        [ { mapKey = "Amnesia Begin", mapValue = "[[d100]]" }
        , { mapKey = "Amnesia End", mapValue = "[[d12]]" }
        ]

let tacticalSeekerRifle =
      { name = "Tactical Seeker Rifle"
      , attributeSafeName = "tactical_seeker_rifle"
      , damageRoll = "2d8"
      , damageType = "P"
      , critEffect = None Text
      , ammo = Some { capacity = 16, charges = 1 }
      }

let lfdPulseGauntlet =
      { name = "LFD Pulse Guantlet"
      , attributeSafeName = "lfd_pulse_guantlet"
      , damageRoll = "2d6"
      , damageType = "B&So"
      , critEffect = Some "Knockdown"
      , ammo = None { capacity : Natural, charges : Natural }
      }

let combatRifle =
      { name = "Combat Rifle"
      , attributeSafeName = "combat_rifle"
      , damageRoll = "3d8"
      , damageType = "P"
      , critEffect = None Text
      , ammo = Some { capacity = 24, charges = 1 }
      }

let buzzbladeDuelingSword =
      { name = "Buzzblade Dueling Sword"
      , attributeSafeName = "buzzblade_dueling_sword"
      , damageRoll = "2d6"
      , damageType = "S"
      , critEffect = None Text
      , ammo = None { capacity : Natural, charges : Natural }
      }

let yellowStarPlasmaRifle =
    -- Unwieldy & Line
      { name = "Yellow Star Plasma Rifle"
      , attributeSafeName = "yellow_star_plasma_rifle"
      , damageRoll = "2d10"
      , damageType = "E&F"
      , critEffect = Some "Burn [[1d8]]"
      , ammo = Some { capacity = 80, charges = 4 }
      }

let tacticalAutobeamRifle =
      { name = "Tactical Autobeam Rifle"
      , attributeSafeName = "tactical_autobeam_rifle"
      , damageRoll = "5d4"
      , damageType = "F"
      , critEffect = Some "Burn [[2d4]]"
      , ammo = Some { capacity = 160, charges = 4 }
      }

let shockGrenade3 =
      { name = "Shock Grenade III"
      , attributeSafeName = "shock_grenade_iii"
      , damageRoll = "3d12"
      , damageType = "E"
      , critEffect = Some "NEVER CRITS.  15ft.  REF 18 half damage"
      , ammo = None { capacity : Natural, charges : Natural }
      }

let redStarPlasmaSword =
      { name = "Red Star Plasma Sword"
      , attributeSafeName = "red_star_plasma_sword"
      , damageRoll = "4d8"
      , damageType = "E&F"
      , critEffect = Some "severe wound (DC 21)."
      , ammo = None { capacity : Natural, charges : Natural }
      }

let auroraArcPistol =
      { name = "Aurora Arc Pistol"
      , attributeSafeName = "aurora_arc_pistol"
      , damageRoll = "3d6"
      , damageType = "E"
      , critEffect = Some "arc [[2d6]]"
      , ammo = Some { capacity = 80, charges = 2 }
      }

in  { basicAttacksAndChecksWithWeapon, trickAttackSimpleAst }
