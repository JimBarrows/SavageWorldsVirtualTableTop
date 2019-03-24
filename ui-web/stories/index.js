import {action}       from '@storybook/addon-actions'
import {storiesOf}    from '@storybook/react'
import React          from 'react'
import Characters     from '../src/components/PlotPointEditor/characters'
import SelectedEdges  from '../src/components/PlotPointEditor/components/character_sheet/selected_edges'
import SelectedSkills from '../src/components/PlotPointEditor/components/character_sheet/selected_skills'
import Navigation     from '../src/components/PlotPointEditor/Navigation'
import PlotPoint      from '../src/components/PlotPointEditor/PlotPoint'
import SettingRules   from '../src/components/PlotPointEditor/setting_rules/index'

let plotPoint = {
	aircraft               : [],
	ammunition             : [],
	arcaneBackgrounds      : [],
	armor                  : [],
	beasts                 : [],
	characters             : [],
	display_section        : 'main',
	description            : ' ',
	edges                  : [],
	groundVehicles         : [],
	handWeapons            : [],
	hindrances             : [],
	maximumAttributePoints : 5,
	maximumMajorHindrances : 1,
	maximumMinorHindrances : 2,
	maximumSkillPoints     : 15,
	mundaneItems           : [],
	name                   : ' ',
	powers                 : [],
	races                  : [],
	rangedWeapons          : [],
	settingRules           : [{
		name       : 'Rule 1',
		description: 'Rule 1 description'
	}],
	skills                 : [],
	specialWeapons         : [],
	trappingsAndEffects    : [],
	vehicleMountedAndAtGuns: [],
	watercraft             : []
}

storiesOf('Plot Point Editor/Navigation', module)
	.addDecorator((story) => <div className="container-fluid" >
		<div className={'row'} >{story()}</div >
	</div >)
	.add('Basic', () => <Navigation id={'basic'} navigateTo={action('Basic Navigation changed')} />)

storiesOf('Plot Point Editor/Form', module)
	.addDecorator((story) => <div className="container-fluid" >
		<div className={'row'} >{story()}</div >
	</div >)
	.add('Plot Point', () => <PlotPoint id={'plotpoint'} onChange={pp => action(`pp: ${pp}`)} plotPoint={plotPoint}
		show={'PlotPoint'} />)
	.add('Setting Rules', () => <SettingRules id={'settingrules'} onChange={pp => action(`pp: ${pp}`)}
		plotPoint={plotPoint} show={'SettingRules'} />)
	.add('Characters', () => <Characters id={'characters'} characters={plotPoint.characters}
		charactersChange={characters => plotPoint = Object.assign({}, plotPoint, {characters})}
		skills={plotPoint.skills} />)

storiesOf('Plot Point Editor/Character Components', module)
	.addDecorator((story) => <div className="container-fluid" >
		<div className={'row'} >{story()}</div >
	</div >)
	.add('Skills', () => <SelectedSkills
		id='selectedskilltest'
		skills={[
			{
				skill: {
					name       : 'Strength Skill',
					description: 'Strength skill description',
					attribute  : 'Strength'
				},
				rank : {
					dice : 'd4',
					bonus: null
				},
				note : 'This is a note'
			}
		]}
		skillsAvailable={[
			{
				name       : 'Agility Skill',
				description: 'Agility skill description',
				attribute  : 'Agility'
			}, {
				name       : 'Strength Skill',
				description: 'Strength skill description',
				attribute  : 'Strength'
			}
		]}
		onChange={action('skills changed')} />)
	.add('Edges', () => <SelectedEdges
		id='selectededgetest'
		edges={[
			{
				edge: {
					name        : 'Edge 1',
					description : 'Edge 1 description',
					category    : 'Category 1',
					requirements: 'requirements 1',
					effects     : 'Effects 1'
				},
				note: 'This is a note'
			}
		]}
		edgesAvailable={[
			{
				name        : 'Edge 1',
				description : 'Edge 1 description',
				category    : 'Category 1',
				requirements: 'requirements 1',
				effects     : 'Effects 1'

			}, {
				name        : 'Edge 2',
				description : 'Edge 2 description',
				category    : 'Category 2',
				requirements: 'Requirements 2',
				effects     : 'Effects 2'
			}
		]}
		onChange={action('edges changed')} />)




