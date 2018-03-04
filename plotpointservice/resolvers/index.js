import ammunitionCategoryQuery from './ammunitionCategoryQuery'
import ammunitionNotesQuery from './ammunitionNotesQuery'
import ammunitionQuery from './ammunitionQuery'
import arcaneBackgroundQuery from './arcaneBackgroundQuery'
import arcaneBackgroundSkillQuery from './arcaneBackgroundSkillQuery'
import armorCategoryQuery from './armorCategoryQuery'
import armorNotesQuery from './armorNotesQuery'
import armorQuery from './armorQuery'
import armorTechnologyLevelQuery from './armorTechnologyLevelQuery'
import beastQuery from './beastQuery'
import beastSkillQuery from './beastSkillQuery'
import characterAmmunitionQuery from './characterAmmunitionQuery'
import characterEdgesQuery from './characterEdgesQuery'
import characterHandWeapons from './characterHandWeaponsQuery'
import characterHindrancesQuery from './characterHindrancesQuery'
import characterMundaneItemsQuery from './characterMundaneItemsQuery'
import characterPowerQuery from './characterPowerQuery'
import characterQuery from './characterQuery'
import edgeRequirementsQuery from './edgeRequirementsQuery'
import edgesResolver from './edgesResolver'
import edgeTypeQuery from './edgeTypeQuery'
import handWeaponCategoryQuery from './handWeaponCategoryQuery'
import handWeaponNotesQuery from './handWeaponNotesQuery'
import handWeaponsResolver from './handWeaponsResolver'
import handWeaponTechnologyQuery from './handWeaponTechnologyQuery'
import hindrancesResolver from './hindrancesResolver'
import monstrousAbilityQuery from './monstrousAbilityQuery'
import mundaneItemCategoryQuery from './mundaneItemCategoryQuery'
import mundaneItemNotesQuery from './mundaneItemNotesQuery'
import mundaneItemsResolver from './mundaneItemsResolver'
import mundaneItemTechnologyQuery from './mundaneItemTechnologyQuery'
import plotPointCreateResolver from './plotPointCreateResolver'
import plotPointQuery from './plotPointQuery'
import powerQuery from './powerQuery'
import powersResolver from './powersResolver'
import skillsResolver from './skillsResolver'
import trappingEffectResolver from './trappingEffectResolver'
import trappingQuery from './trappingQuery'

export default {
  Ammunition: {
    category: ammunitionCategoryQuery,
    notes: ammunitionNotesQuery,
  },
  AmmunitionAmount: {
    notes: ammunitionNotesQuery,
    category: ammunitionCategoryQuery,
  },
  ArcaneBackground: {
    skill: arcaneBackgroundSkillQuery
  },
  Armor: {
    category: armorCategoryQuery,
    notes: armorNotesQuery,
    technology_level: armorTechnologyLevelQuery
  },
  Beast: {
    skills: beastSkillQuery,
    special_abilities: monstrousAbilityQuery
  },
  Character: {
    ammunition: characterAmmunitionQuery,
    edges: characterEdgesQuery,
    hand_weapons: characterHandWeapons,
    hindrances: characterHindrancesQuery,
    mundane_items: characterMundaneItemsQuery,
    powers: characterPowerQuery
  },
  CharacterPower: {
    power: powerQuery,
    trapping: trappingQuery
  },
  Edge: {
    requirements: edgeRequirementsQuery,
    type: edgeTypeQuery
  },
  HandWeapon: {
    category: handWeaponCategoryQuery,
    notes: handWeaponNotesQuery,
    technology_level: handWeaponTechnologyQuery
  },
  MundaneItem: {
    category: mundaneItemCategoryQuery,
    notes: mundaneItemNotesQuery,
    technology_level: mundaneItemTechnologyQuery
  },
  Mutation: {
    plot_point_create: plotPointCreateResolver
  },
  PlotPoint: {
    ammunition: ammunitionQuery,
    arcaneBackgrounds: arcaneBackgroundQuery,
    armors: armorQuery,
    beasts: beastQuery,
    characters: characterQuery,
    edges: edgesResolver,
    hand_weapons: handWeaponsResolver,
    hindrances: hindrancesResolver,
    mundane_items: mundaneItemsResolver,
    powers: powersResolver,
    skills: skillsResolver
  },
  Query: {
    plotPoints: plotPointQuery
  },
  Trapping: {
    effects: trappingEffectResolver
  }
}