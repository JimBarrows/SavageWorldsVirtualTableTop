export default [{
  "id": "3ca3b984-ee2f-44bd-a0ec-bae2d09d3f44",
  "name": "plot_point_1",
  "description": "plot point 1 description",
  "brief_description": "plot point 1 brief description",
  "ammunition": [
    {
      "id": "73a094aa-8e65-40af-95b3-53eca3e62774",
      "name": "Ammunition 1",
      "description": "ammunition 1 description",
      "weight": 1,
      "cost": 1,
      "category": {
        "id": "a22b4c0f-5177-47a6-8bb2-603cea8a89f7",
        "name": "Ammunition 1 category name",
        "description": "Ammunition 1 category description"
      },
      "notes": [
        {
          "id": "9082fdc9-9a05-402d-bca3-a24f9e1162a0",
          "name": "Ammunition Gear note 1",
          "description": "Ammunition gear note description 1"
        }
      ]
    }
  ],
  "arcaneBackgrounds": [
    {
      "id": "9375899f-7e07-4d2f-b934-4e68cbde3a9b",
      "name": "arcane_background 1 name",
      "description": "arcane_background 1 description",
      "skill": {
        "id": "edb18f12-5bb5-44fb-bf97-59a6e9d684d7",
        "name": "Skills 1",
        "description": "Skill description 1",
        "ability": "strength"
      },
      "starting_power_points": 1,
      "starting_powers": 2
    }
  ],
  "armors": [
    {
      "id": "7064a160-f074-4d6d-9244-14065232aafb",
      "name": "Armor 1",
      "description": "Armor 1 description",
      "weight": 1,
      "cost": 1,
      "category": {
        "id": "94da51df-44a6-48db-a597-78293351691e",
        "name": "Armor 1 category name",
        "description": "Armor 1 category description"
      },
      "notes": [
        {
          "id": "1d5a0d8f-0a74-418b-afba-ab4da79d29de",
          "name": "Armor Gear note 1",
          "description": "armor gear note description 1"
        }
      ],
      "technology_level": {
        "id": "86e7508b-b419-47d7-94ab-43fce84ec361",
        "name": "Armor 1 technology_level name",
        "description": "Armor 1 technology_level description"
      },
      "armor": 1
    }
  ],
  "beasts": [
    {
      "id": "85c4ffc3-6092-4bca-a9fe-1f019bc54b91",
      "name": "Beast 1",
      "agility": "d4",
      "smarts": "d6",
      "spirit": "d8",
      "strength": "d10",
      "vigor": "d12",
      "animal_intelligence": true,
      "pace": 1,
      "parry": 2,
      "toughness": 3,
      "armor": 4,
      "wild_card": true,
      "description": "Beast description 1",
      "skills": [
        {
          "id": "edb18f12-5bb5-44fb-bf97-59a6e9d684d7",
          "name": "Skills 1",
          "description": "Skill description 1",
          "ability": "strength",
          "value": "d4",
          "bonus": 0
        }
      ],
      "special_abilities": [
        {
          id: "ebe21ebd-f98e-4c89-8022-1c100a8a9045",
          name: "Monstrous_Ability 1",
          description: "Monstrous_Ability description 1"
        }
      ]
    }
  ],
  "characters": [
    {
      "id": "8c5d3873-0084-4127-99cd-322bc7fba06c",
      "agility": "d4",
      "ammunition": [
        {
          "id": "73a094aa-8e65-40af-95b3-53eca3e62774",
          "name": "Ammunition 1",
          "description": "ammunition 1 description",
          "weight": 1,
          "cost": 1,
          "amount": 1,
          "category": {
            "id": "a22b4c0f-5177-47a6-8bb2-603cea8a89f7",
            "name": "Ammunition 1 category name",
            "description": "Ammunition 1 category description"
          },
          "notes": [
            {
              "id": "9082fdc9-9a05-402d-bca3-a24f9e1162a0",
              "name": "Ammunition Gear note 1",
              "description": "Ammunition gear note description 1"
            }
          ]
        }
      ],
      "armor": 1,
      "charisma": 2,
      "description": "Beast description 1",
      "edges": [
        {
          "id": "63d400c4-4789-4a05-b31d-028310c57f8e",
          "name": "Edge 1",
          "description": "Edge description 1",
          "effects": "edge effects 1",
          "requirements": [
            {
              "id": "775c330d-c836-4d82-ba4c-9e220c578baf",
              "requirement": "Edge 1 requirement 1"
            }
          ],
          "type": {
            "id": "50c5482d-6622-46b5-87a8-43d78dbc3090",
            "name": "Edge type 1",
            "description": "Edge type description 1"
          }
        }
      ],
      "experience_points": 3,
      "hand_weapons": [
        {
          "id": "e0d242e8-9fe0-4963-8b5f-f710f64b52b2",
          "category": {
            "id": "57f784bb-7f34-43ad-85a5-f2f090f268fd",
            "name": "Hand Weapon 1 category name",
            "description": "Hand WEapon 1 category description"
          },
          "cost": 1,
          "description": "Hand Weapon 1 description",
          "name": "Hand Weapon 1",
          "notes": [
            {
              "id": "a1b30728-7f4a-40a8-86d3-2ff99db103be",
              "name": "Hand Weapon 1 Note",
              "description": "Hand Weapon 1 Note Description"
            }
          ],
          "technology_level": {
            "id": "e94541d8-66d4-4dff-a621-5dce91418d46",
            "name": "Hand Weapon 1 Technology Level",
            "description": "Hand Weapon 1 Technology Level description"
          },
          "weight": 2,
          "damage": "damage",
          "amount": 1
        }
      ],
      "hindrances": [
        {
          "id": "c86b5090-fa86-4a6f-aea3-5d3f1d3c45dc",
          "name": "Hindrance 1",
          "description": "Hindrance description 1",
          "type": "minor",
          "taken_as": "minor"
        }
      ],
      "mundane_items": [
        {
          "id": "a6314b6e-3ee5-47eb-8a8e-ccd11c689ee6",
          "category": {
            "id": "07e8b13a-4cfb-4c2c-9ce2-b2044b53daea",
            "name": "Mundane Item 1 category name",
            "description": "Mundane Item 1 category description"
          },
          "cost": 1,
          "description": "Mundane Item 1 description",
          "name": "Mundane Item 1",
          "notes": [
            {
              "id": "3b75e460-6ca8-4f67-abfd-ce4e103fb068",
              "name": "Mundane Item 1 Note",
              "description": "Mundane Item 1 Note Description"
            }
          ],
          "technology_level": {
            "id": "c65dd88e-d3f4-4e45-b7ff-d8ebb555b04e",
            "name": "Mundane Item 1 Technology Level",
            "description": "Mundane Item 1 Technology Level description"
          },
          "weight": 2
        }
      ],
      "name": "Character 1",
      "pace": 5,
      "parry": 6,
      "powers": [
        {
          "id": "c13cdc8d-d5b5-4bfb-b270-5263d69d6f6b",
          "name": "Character Power 1",
          "notes": "Character Power 1 note",
          "power": {
            "id": "74d3c9d9-1ab9-4eea-a92c-36740977f031",
            "name": "Power 1 name",
            "description": "Power 1 description",
            "rank": "novice",
            "power_points": 1,
            "range": "Power 1 range",
            "duration": "Power 1 duration",
            "trappings": "Power 1 trappings",
            "available_to": {
              "id": "9375899f-7e07-4d2f-b934-4e68cbde3a9b",
              "name": "arcane_background 1 name",
              "description": "arcane_background 1 description",
              "skill": {
                "id": "edb18f12-5bb5-44fb-bf97-59a6e9d684d7",
                "name": "Skills 1",
                "description": "Skill description 1",
                "ability": "strength",
                "value": "d4",
                "bonus": 0
              }
            }
          },
          "trapping": {
            "id": "fce6246c-01d6-452d-9b46-bb959cb6a60a",
            "name": "Trapping 1",
            "description": "Trapping 1 description",
            "effects": [
              {
                "id": "2128b662-034a-4b0d-a4dc-1287cad34c28",
                "name": "Traping effect 1",
                "description": "Trapping effect 1 description"
              }
            ]
          }
        }
      ],
      "power_points": 7,
      "rank": "novice",
      "smarts": "d6",
      "spirit": "d8",
      "strength": "d10",
      "toughness": 8,
      "vigor": "d12",
      "wild_card": true
    }],
  "edges": [
    {
      "id": "63d400c4-4789-4a05-b31d-028310c57f8e",
      "name": "Edge 1",
      "description": "Edge description 1",
      "effects": "edge effects 1",
      "requirements": [
        {
          "id": "775c330d-c836-4d82-ba4c-9e220c578baf",
          "requirement": "Edge 1 requirement 1"
        }
      ],
      "type": {
        "id": "50c5482d-6622-46b5-87a8-43d78dbc3090",
        "name": "Edge type 1",
        "description": "Edge type description 1"
      }
    }
  ],
  "hand_weapons": [
    {
      "id": "e0d242e8-9fe0-4963-8b5f-f710f64b52b2",
      "category": {
        "id": "57f784bb-7f34-43ad-85a5-f2f090f268fd",
        "name": "Hand Weapon 1 category name",
        "description": "Hand WEapon 1 category description"
      },
      "cost": 1,
      "description": "Hand Weapon 1 description",
      "name": "Hand Weapon 1",
      "notes": [
        {
          "id": "a1b30728-7f4a-40a8-86d3-2ff99db103be",
          "name": "Hand Weapon 1 Note",
          "description": "Hand Weapon 1 Note Description"
        }
      ],
      "technology_level": {
        "id": "e94541d8-66d4-4dff-a621-5dce91418d46",
        "name": "Hand Weapon 1 Technology Level",
        "description": "Hand Weapon 1 Technology Level description"
      },
      "weight": 2,
      "damage": "damage"
    }
  ],
  "hindrances": [
    {
      "id": "c86b5090-fa86-4a6f-aea3-5d3f1d3c45dc",
      "name": "Hindrance 1",
      "description": "Hindrance description 1",
      "type": "minor"
    }
  ],
  "monstrous_abilities": [
    {
      id: "ebe21ebd-f98e-4c89-8022-1c100a8a9045",
      name: "Monstrous_Ability 1",
      description: "Monstrous_Ability description 1"
    }
  ],
  "mundane_items": [
    {
      "id": "a6314b6e-3ee5-47eb-8a8e-ccd11c689ee6",
      "category": {
        "id": "07e8b13a-4cfb-4c2c-9ce2-b2044b53daea",
        "name": "Mundane Item 1 category name",
        "description": "Mundane Item 1 category description"
      },
      "cost": 1,
      "description": "Mundane Item 1 description",
      "name": "Mundane Item 1",
      "notes": [
        {
          "id": "3b75e460-6ca8-4f67-abfd-ce4e103fb068",
          "name": "Mundane Item 1 Note",
          "description": "Mundane Item 1 Note Description"
        }
      ],
      "technology_level": {
        "id": "c65dd88e-d3f4-4e45-b7ff-d8ebb555b04e",
        "name": "Mundane Item 1 Technology Level",
        "description": "Mundane Item 1 Technology Level description"
      },
      "weight": 2
    }
  ],
  "powers": [
    {
      "id": "74d3c9d9-1ab9-4eea-a92c-36740977f031",
      "name": "Power 1 name",
      "description": "Power 1 description",
      "rank": "novice",
      "power_points": 1,
      "range": "Power 1 range",
      "duration": "Power 1 duration",
      "trappings": "Power 1 trappings",
      "available_to": {
        "id": "9375899f-7e07-4d2f-b934-4e68cbde3a9b",
        "name": "arcane_background 1 name",
        "description": "arcane_background 1 description",
        "skill": {
          "id": "edb18f12-5bb5-44fb-bf97-59a6e9d684d7",
          "name": "Skills 1",
          "description": "Skill description 1",
          "ability": "strength",
          "value": "d4",
          "bonus": 0
        },
        "starting_power_points": 1,
        "starting_powers": 2
      }
    }
  ],
  "skills": [
    {
      "id": "edb18f12-5bb5-44fb-bf97-59a6e9d684d7",
      "name": "Skills 1",
      "description": "Skill description 1",
      "ability": "strength"
    }
  ],
  "trappings": [
    {
      "id": "fce6246c-01d6-452d-9b46-bb959cb6a60a",
      "name": "Trapping 1",
      "description": "Trapping 1 description",
      "effects": [
        {
          "id": "2128b662-034a-4b0d-a4dc-1287cad34c28",
          "name": "Traping effect 1",
          "description": "Trapping effect 1 description"
        }
      ]
    }
  ]
}]