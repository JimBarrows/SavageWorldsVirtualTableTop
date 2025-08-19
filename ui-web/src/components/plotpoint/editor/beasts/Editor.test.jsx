import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import Editor from './Editor';

// Mock the dependencies
jest.mock('bootstrap-react-components', () => ({
  NumberFormGroup: ({ id, label, onChange, value, required }) => (
    <div data-testid={`number-form-${id}`}>
      <label>{label}</label>
      <input
        type="number"
        onChange={onChange}
        value={value || ''}
        required={required}
        data-testid={`number-input-${id}`}
      />
    </div>
  ),
  TextAreaFormGroup: ({ id, label, onChange, value, required }) => (
    <div data-testid={`textarea-form-${id}`}>
      <label>{label}</label>
      <textarea
        onChange={onChange}
        value={value || ''}
        required={required}
        data-testid={`textarea-${id}`}
      />
    </div>
  ),
  TextFormGroup: ({ id, label, onChange, value, required }) => (
    <div data-testid={`text-form-${id}`}>
      <label>{label}</label>
      <input
        type="text"
        onChange={onChange}
        value={value || ''}
        required={required}
        data-testid={`text-input-${id}`}
      />
    </div>
  ),
  CheckboxFormGroup: ({ id, label, onChange, checked }) => (
    <div data-testid={`checkbox-form-${id}`}>
      <label>{label}</label>
      <input
        type="checkbox"
        onChange={onChange}
        checked={checked || false}
        data-testid={`checkbox-input-${id}`}
      />
    </div>
  ),
  PrependAddon: ({ children }) => <div data-testid="prepend-addon">{children}</div>,
  Button: ({ onClick, children, id }) => (
    <button onClick={onClick} data-testid={`button-${id}`}>
      {children}
    </button>
  ),
  ListGroup: ({ children, id }) => (
    <div data-testid={`list-group-${id}`}>{children}</div>
  )
}));

// Mock CharacterSheet component
jest.mock('../../../character_sheet/index', () => {
  return function MockCharacterSheet({ id, index, item, skillsAvailable, onChange, children }) {
    return (
      <div data-testid={`character-sheet-${id}`}>
        <div data-testid="character-sheet-name">
          <label>Name</label>
          <input
            type="text"
            onChange={(e) => onChange({ ...item, name: e.target.value })}
            value={item.name || ''}
            data-testid="character-sheet-name-input"
          />
        </div>
        <div data-testid="character-sheet-description">
          <label>Description</label>
          <textarea
            onChange={(e) => onChange({ ...item, description: e.target.value })}
            value={item.description || ''}
            data-testid="character-sheet-description-input"
          />
        </div>
        <div data-testid="character-sheet-agility">
          <label>Agility</label>
          <input
            type="text"
            onChange={(e) => onChange({ ...item, agility: e.target.value })}
            value={item.agility || ''}
            data-testid="character-sheet-agility-input"
          />
        </div>
        <div data-testid="character-sheet-smarts">
          <label>Smarts</label>
          <input
            type="text"
            onChange={(e) => onChange({ ...item, smarts: e.target.value })}
            value={item.smarts || ''}
            data-testid="character-sheet-smarts-input"
          />
        </div>
        <div data-testid="character-sheet-spirit">
          <label>Spirit</label>
          <input
            type="text"
            onChange={(e) => onChange({ ...item, spirit: e.target.value })}
            value={item.spirit || ''}
            data-testid="character-sheet-spirit-input"
          />
        </div>
        <div data-testid="character-sheet-strength">
          <label>Strength</label>
          <input
            type="text"
            onChange={(e) => onChange({ ...item, strength: e.target.value })}
            value={item.strength || ''}
            data-testid="character-sheet-strength-input"
          />
        </div>
        <div data-testid="character-sheet-vigor">
          <label>Vigor</label>
          <input
            type="text"
            onChange={(e) => onChange({ ...item, vigor: e.target.value })}
            value={item.vigor || ''}
            data-testid="character-sheet-vigor-input"
          />
        </div>
        <div data-testid="character-sheet-pace">
          <label>Pace</label>
          <input
            type="number"
            onChange={(e) => onChange({ ...item, pace: parseInt(e.target.value, 10) })}
            value={item.pace || ''}
            data-testid="character-sheet-pace-input"
          />
        </div>
        <div data-testid="character-sheet-parry">
          <label>Parry</label>
          <input
            type="number"
            onChange={(e) => onChange({ ...item, parry: parseInt(e.target.value, 10) })}
            value={item.parry || ''}
            data-testid="character-sheet-parry-input"
          />
        </div>
        <div data-testid="character-sheet-toughness">
          <label>Toughness</label>
          <input
            type="number"
            onChange={(e) => onChange({ ...item, toughness: parseInt(e.target.value, 10) })}
            value={item.toughness || ''}
            data-testid="character-sheet-toughness-input"
          />
        </div>
        <div data-testid="skills-available">
          Skills Available: {skillsAvailable.length}
        </div>
        {children}
      </div>
    );
  };
});

// Mock SpecialAbilities component
jest.mock('./special_abilities/index', () => {
  return function MockSpecialAbilities({ abilities, id, onChange }) {
    return (
      <div data-testid={`special-abilities-${id}`}>
        <h3>Special Abilities</h3>
        <button 
          onClick={() => onChange([...abilities, { name: 'New Ability', description: 'New Description' }])}
          data-testid="add-special-ability"
        >
          Add Special Ability
        </button>
        <div data-testid="abilities-count">
          Abilities: {abilities ? abilities.length : 0}
        </div>
        {abilities && abilities.map((ability, index) => (
          <div key={index} data-testid={`ability-${index}`}>
            <input 
              type="text" 
              value={ability.name || ''} 
              onChange={(e) => {
                const newAbilities = [...abilities];
                newAbilities[index] = { ...ability, name: e.target.value };
                onChange(newAbilities);
              }}
              data-testid={`ability-name-${index}`}
            />
            <textarea 
              value={ability.description || ''} 
              onChange={(e) => {
                const newAbilities = [...abilities];
                newAbilities[index] = { ...ability, description: e.target.value };
                onChange(newAbilities);
              }}
              data-testid={`ability-description-${index}`}
            />
          </div>
        ))}
      </div>
    );
  };
});

describe('Beasts Editor Component', () => {
  const defaultProps = {
    id: 'test-beast-editor',
    index: 0,
    skillsAvailable: [
      { name: 'Fighting', attribute: 'Agility' },
      { name: 'Climbing', attribute: 'Strength' },
      { name: 'Swimming', attribute: 'Agility' },
      { name: 'Stealth', attribute: 'Agility' },
      { name: 'Notice', attribute: 'Smarts' }
    ],
    item: {
      name: 'Wolf',
      description: 'A fierce predator of the wilderness',
      agility: 'd8',
      smarts: 'd4(A)',
      spirit: 'd6',
      strength: 'd8',
      vigor: 'd8',
      pace: 8,
      parry: 6,
      toughness: 6,
      armor: 1,
      specialAbilities: [
        { name: 'Fleet Footed', description: 'Rolls d10 running die instead of d6' },
        { name: 'Go for the Throat', description: 'With raise on attack, hits to head/neck' }
      ]
    },
    onChange: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders without crashing', () => {
      render(<Editor {...defaultProps} />);
      expect(screen.getByTestId('character-sheet-BeastEditor-0-test-beast-editor')).toBeInTheDocument();
    });

    it('renders CharacterSheet component with correct props', () => {
      render(<Editor {...defaultProps} />);
      
      const characterSheet = screen.getByTestId('character-sheet-BeastEditor-0-test-beast-editor');
      expect(characterSheet).toBeInTheDocument();
      
      expect(screen.getByTestId('skills-available')).toHaveTextContent('Skills Available: 5');
    });

    it('renders armor field correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const armorField = screen.getByTestId('number-form-BeastEditor-0-test-beast-editor-Armor');
      expect(armorField).toBeInTheDocument();
      
      const armorInput = screen.getByTestId('number-input-BeastEditor-0-test-beast-editor-Armor');
      expect(armorInput).toHaveValue('1');
    });

    it('renders SpecialAbilities component with correct props', () => {
      render(<Editor {...defaultProps} />);
      
      const specialAbilities = screen.getByTestId('special-abilities-BeastEditor-0-test-beast-editor');
      expect(specialAbilities).toBeInTheDocument();
      
      expect(screen.getByTestId('abilities-count')).toHaveTextContent('Abilities: 2');
    });

    it('displays beast attributes correctly in CharacterSheet', () => {
      render(<Editor {...defaultProps} />);
      
      expect(screen.getByDisplayValue('Wolf')).toBeInTheDocument();
      expect(screen.getByDisplayValue('A fierce predator of the wilderness')).toBeInTheDocument();
      expect(screen.getByDisplayValue('d8')).toBeInTheDocument();
      expect(screen.getByDisplayValue('d4(A)')).toBeInTheDocument();
    });
  });

  describe('State Management', () => {
    it('initializes with correct default state', () => {
      const component = new Editor(defaultProps);
      expect(component.state.selected).toBe('');
    });

    it('can update state', () => {
      const component = new Editor(defaultProps);
      component.setState({ selected: 'test' });
      expect(component.state.selected).toBe('test');
    });
  });

  describe('Armor Field Updates', () => {
    it('updates armor value correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const armorInput = screen.getByTestId('number-input-BeastEditor-0-test-beast-editor-Armor');
      fireEvent.change(armorInput, { target: { value: '3' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, armor: 3 },
        0
      );
    });

    it('handles zero armor value', () => {
      render(<Editor {...defaultProps} />);
      
      const armorInput = screen.getByTestId('number-input-BeastEditor-0-test-beast-editor-Armor');
      fireEvent.change(armorInput, { target: { value: '0' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, armor: 0 },
        0
      );
    });

    it('handles empty string for armor as 0', () => {
      render(<Editor {...defaultProps} />);
      
      const armorInput = screen.getByTestId('number-input-BeastEditor-0-test-beast-editor-Armor');
      fireEvent.change(armorInput, { target: { value: '' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, armor: 0 },
        0
      );
    });

    it('handles non-numeric input for armor as 0', () => {
      render(<Editor {...defaultProps} />);
      
      const armorInput = screen.getByTestId('number-input-BeastEditor-0-test-beast-editor-Armor');
      fireEvent.change(armorInput, { target: { value: 'abc' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, armor: 0 },
        0
      );
    });

    it('handles missing armor property with default value', () => {
      const propsWithoutArmor = {
        ...defaultProps,
        item: { ...defaultProps.item }
      };
      delete propsWithoutArmor.item.armor;
      
      render(<Editor {...propsWithoutArmor} />);
      
      const armorInput = screen.getByTestId('number-input-BeastEditor-0-test-beast-editor-Armor');
      expect(armorInput).toHaveValue('0');
    });
  });

  describe('Character Sheet Integration', () => {
    it('passes item changes to CharacterSheet correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('character-sheet-name-input');
      fireEvent.change(nameInput, { target: { value: 'Dire Wolf' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, name: 'Dire Wolf' },
        0
      );
    });

    it('handles attribute updates correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const agilityInput = screen.getByTestId('character-sheet-agility-input');
      fireEvent.change(agilityInput, { target: { value: 'd10' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, agility: 'd10' },
        0
      );
    });

    it('handles derived stats updates correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const paceInput = screen.getByTestId('character-sheet-pace-input');
      fireEvent.change(paceInput, { target: { value: '10' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, pace: 10 },
        0
      );
    });
  });

  describe('Special Abilities Updates', () => {
    it('updates special abilities correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const addButton = screen.getByTestId('add-special-ability');
      fireEvent.click(addButton);
      
      const expectedAbilities = [
        ...defaultProps.item.specialAbilities,
        { name: 'New Ability', description: 'New Description' }
      ];
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, specialAbilities: expectedAbilities },
        0
      );
    });

    it('handles ability name changes', () => {
      render(<Editor {...defaultProps} />);
      
      const abilityNameInput = screen.getByTestId('ability-name-0');
      fireEvent.change(abilityNameInput, { target: { value: 'Updated Fleet Footed' } });
      
      const expectedAbilities = [
        { name: 'Updated Fleet Footed', description: 'Rolls d10 running die instead of d6' },
        { name: 'Go for the Throat', description: 'With raise on attack, hits to head/neck' }
      ];
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, specialAbilities: expectedAbilities },
        0
      );
    });

    it('handles ability description changes', () => {
      render(<Editor {...defaultProps} />);
      
      const abilityDescInput = screen.getByTestId('ability-description-1');
      fireEvent.change(abilityDescInput, { target: { value: 'Updated throat attack description' } });
      
      const expectedAbilities = [
        { name: 'Fleet Footed', description: 'Rolls d10 running die instead of d6' },
        { name: 'Go for the Throat', description: 'Updated throat attack description' }
      ];
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, specialAbilities: expectedAbilities },
        0
      );
    });
  });

  describe('Savage Worlds Beast Types', () => {
    const savageWorldsBeasts = [
      {
        name: 'Wolf',
        agility: 'd8',
        smarts: 'd4(A)',
        spirit: 'd6',
        strength: 'd8',
        vigor: 'd8',
        armor: 1,
        specialAbilities: [
          { name: 'Fleet Footed', description: 'Rolls d10 running die instead of d6' },
          { name: 'Go for the Throat', description: 'With raise on attack, hits to head/neck' }
        ]
      },
      {
        name: 'Bear',
        agility: 'd6',
        smarts: 'd4(A)',
        spirit: 'd8',
        strength: 'd12+2',
        vigor: 'd10',
        armor: 1,
        specialAbilities: [
          { name: 'Size +2', description: 'Large creature' },
          { name: 'Claws', description: 'Str+d6 damage' }
        ]
      },
      {
        name: 'Dragon (Ancient)',
        agility: 'd6',
        smarts: 'd12',
        spirit: 'd12',
        strength: 'd12+5',
        vigor: 'd12',
        armor: 6,
        specialAbilities: [
          { name: 'Size +8', description: 'Huge creature' },
          { name: 'Fire Breath', description: '3d10 damage in cone template' },
          { name: 'Flight', description: 'Can fly at pace 12' },
          { name: 'Hardy', description: 'Doesn\'t suffer wound penalties' }
        ]
      }
    ];

    it('handles standard Savage Worlds beasts', () => {
      savageWorldsBeasts.forEach((beast, index) => {
        const props = { ...defaultProps, item: beast, id: `beast-${index}` };
        render(<Editor {...props} />);
        
        expect(screen.getByDisplayValue(beast.name)).toBeInTheDocument();
        expect(screen.getByDisplayValue(beast.agility)).toBeInTheDocument();
        expect(screen.getByDisplayValue(beast.strength)).toBeInTheDocument();
        
        const abilitiesCount = screen.getByTestId('abilities-count');
        expect(abilitiesCount).toHaveTextContent(`Abilities: ${beast.specialAbilities.length}`);
      });
    });

    it('handles animal intelligence correctly', () => {
      const animalBeast = {
        ...defaultProps.item,
        smarts: 'd4(A)',
        animalIntelligence: true
      };
      
      const props = { ...defaultProps, item: animalBeast };
      render(<Editor {...props} />);
      
      expect(screen.getByDisplayValue('d4(A)')).toBeInTheDocument();
    });

    it('handles size modifiers in stats', () => {
      const largeBeast = {
        ...defaultProps.item,
        name: 'Giant Spider',
        strength: 'd12+2',
        toughness: '9(2)',
        specialAbilities: [
          { name: 'Size +1', description: 'Large creature' },
          { name: 'Poison', description: 'Vigor roll or paralyzed' }
        ]
      };
      
      const props = { ...defaultProps, item: largeBeast };
      render(<Editor {...props} />);
      
      expect(screen.getByDisplayValue('Giant Spider')).toBeInTheDocument();
      expect(screen.getByDisplayValue('d12+2')).toBeInTheDocument();
    });
  });

  describe('Skills Available Integration', () => {
    it('passes skillsAvailable to CharacterSheet correctly', () => {
      render(<Editor {...defaultProps} />);
      
      expect(screen.getByTestId('skills-available')).toHaveTextContent('Skills Available: 5');
    });

    it('handles empty skillsAvailable array', () => {
      const propsWithNoSkills = { ...defaultProps, skillsAvailable: [] };
      render(<Editor {...propsWithNoSkills} />);
      
      expect(screen.getByTestId('skills-available')).toHaveTextContent('Skills Available: 0');
    });

    it('handles different skill sets', () => {
      const combatSkills = [
        { name: 'Fighting', attribute: 'Agility' },
        { name: 'Shooting', attribute: 'Agility' },
        { name: 'Throwing', attribute: 'Agility' }
      ];
      
      const propsWithCombatSkills = { ...defaultProps, skillsAvailable: combatSkills };
      render(<Editor {...propsWithCombatSkills} />);
      
      expect(screen.getByTestId('skills-available')).toHaveTextContent('Skills Available: 3');
    });
  });

  describe('Edge Cases', () => {
    it('handles undefined item properties gracefully', () => {
      const propsWithUndefined = {
        ...defaultProps,
        item: {}
      };
      
      expect(() => render(<Editor {...propsWithUndefined} />)).not.toThrow();
    });

    it('handles null special abilities gracefully', () => {
      const propsWithNullAbilities = {
        ...defaultProps,
        item: { ...defaultProps.item, specialAbilities: null }
      };
      
      render(<Editor {...propsWithNullAbilities} />);
      expect(screen.getByTestId('abilities-count')).toHaveTextContent('Abilities: 0');
    });

    it('handles undefined special abilities gracefully', () => {
      const propsWithUndefinedAbilities = {
        ...defaultProps,
        item: { ...defaultProps.item }
      };
      delete propsWithUndefinedAbilities.item.specialAbilities;
      
      render(<Editor {...propsWithUndefinedAbilities} />);
      expect(screen.getByTestId('abilities-count')).toHaveTextContent('Abilities: 0');
    });

    it('handles different index values', () => {
      const props = { ...defaultProps, index: 5 };
      render(<Editor {...props} />);
      
      const armorInput = screen.getByTestId('number-input-BeastEditor-5-test-beast-editor-Armor');
      fireEvent.change(armorInput, { target: { value: '2' } });
      
      expect(defaultProps.onChange).toHaveBeenCalledWith(
        { ...defaultProps.item, armor: 2 },
        5
      );
    });

    it('handles very long descriptions', () => {
      const longDescription = 'A'.repeat(1000);
      const propsWithLongDesc = {
        ...defaultProps,
        item: { ...defaultProps.item, description: longDescription }
      };
      
      render(<Editor {...propsWithLongDesc} />);
      expect(screen.getByDisplayValue(longDescription)).toBeInTheDocument();
    });

    it('handles special characters in beast names', () => {
      const specialName = 'Beast with "quotes" & symbols <>!@#$%^&*()';
      const propsWithSpecialName = {
        ...defaultProps,
        item: { ...defaultProps.item, name: specialName }
      };
      
      render(<Editor {...propsWithSpecialName} />);
      expect(screen.getByDisplayValue(specialName)).toBeInTheDocument();
    });
  });

  describe('Component Integration', () => {
    it('correctly generates component IDs', () => {
      render(<Editor {...defaultProps} />);
      
      const characterSheet = screen.getByTestId('character-sheet-BeastEditor-0-test-beast-editor');
      const specialAbilities = screen.getByTestId('special-abilities-BeastEditor-0-test-beast-editor');
      const armorField = screen.getByTestId('number-form-BeastEditor-0-test-beast-editor-Armor');
      
      expect(characterSheet).toBeInTheDocument();
      expect(specialAbilities).toBeInTheDocument();
      expect(armorField).toBeInTheDocument();
    });

    it('maintains component state consistency across updates', () => {
      const { rerender } = render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('character-sheet-name-input');
      fireEvent.change(nameInput, { target: { value: 'Updated Wolf' } });
      
      const updatedProps = {
        ...defaultProps,
        item: { ...defaultProps.item, name: 'Updated Wolf' }
      };
      
      rerender(<Editor {...updatedProps} />);
      expect(screen.getByDisplayValue('Updated Wolf')).toBeInTheDocument();
    });

    it('handles multiple instances with different IDs', () => {
      const props1 = { ...defaultProps, id: 'beast-1', index: 0 };
      const props2 = { ...defaultProps, id: 'beast-2', index: 1 };
      
      const { container } = render(
        <div>
          <Editor {...props1} />
          <Editor {...props2} />
        </div>
      );
      
      expect(container.querySelectorAll('[data-testid^="character-sheet-"]')).toHaveLength(2);
      expect(container.querySelectorAll('[data-testid^="special-abilities-"]')).toHaveLength(2);
    });
  });

  describe('Armor Value Validation', () => {
    it('handles typical beast armor values (0-6)', () => {
      for (let armorValue = 0; armorValue <= 6; armorValue++) {
        const props = { ...defaultProps, item: { ...defaultProps.item, armor: armorValue } };
        render(<Editor {...props} />);
        
        const armorInput = screen.getByTestId('number-input-BeastEditor-0-test-beast-editor-Armor');
        expect(armorInput.value).toBe(armorValue.toString());
      }
    });

    it('handles high armor values for dragons and magical beasts', () => {
      const highArmorValues = [8, 10, 12];
      
      highArmorValues.forEach(value => {
        render(<Editor {...defaultProps} />);
        
        const armorInput = screen.getByTestId('number-input-BeastEditor-0-test-beast-editor-Armor');
        fireEvent.change(armorInput, { target: { value: value.toString() } });
        
        expect(defaultProps.onChange).toHaveBeenCalledWith(
          { ...defaultProps.item, armor: value },
          0
        );
        
        jest.clearAllMocks();
      });
    });
  });

  describe('Complex Special Abilities', () => {
    it('handles beasts with many special abilities', () => {
      const complexBeast = {
        ...defaultProps.item,
        name: 'Ancient Dragon',
        specialAbilities: [
          { name: 'Size +8', description: 'Huge creature' },
          { name: 'Flight', description: 'Can fly at pace 12' },
          { name: 'Fire Breath', description: '3d10 damage in cone template' },
          { name: 'Hardy', description: 'Doesn\'t suffer wound penalties' },
          { name: 'Improved Frenzy', description: 'May make 2 attacks per round with no penalty' },
          { name: 'Fearless', description: 'Immune to fear and intimidation' }
        ]
      };
      
      const props = { ...defaultProps, item: complexBeast };
      render(<Editor {...props} />);
      
      expect(screen.getByTestId('abilities-count')).toHaveTextContent('Abilities: 6');
      expect(screen.getByTestId('ability-name-0')).toHaveValue('Size +8');
      expect(screen.getByTestId('ability-name-5')).toHaveValue('Fearless');
    });

    it('handles empty special abilities array', () => {
      const beastWithoutAbilities = {
        ...defaultProps.item,
        specialAbilities: []
      };
      
      const props = { ...defaultProps, item: beastWithoutAbilities };
      render(<Editor {...props} />);
      
      expect(screen.getByTestId('abilities-count')).toHaveTextContent('Abilities: 0');
    });
  });

  describe('Attribute Dice Integration', () => {
    const attributeDiceValues = ['d4', 'd4(A)', 'd6', 'd8', 'd10', 'd12', 'd12+1', 'd12+2', 'd12+3'];

    attributeDiceValues.forEach(diceValue => {
      it(`handles ${diceValue} attribute dice`, () => {
        const beastWithDice = {
          ...defaultProps.item,
          agility: diceValue
        };
        
        const props = { ...defaultProps, item: beastWithDice };
        render(<Editor {...props} />);
        
        expect(screen.getByDisplayValue(diceValue)).toBeInTheDocument();
      });
    });

    it('handles animal intelligence notation', () => {
      const animalBeasts = [
        { smarts: 'd4(A)' },
        { smarts: 'd6(A)' },
        { smarts: 'd8(A)' }
      ];
      
      animalBeasts.forEach(beast => {
        const props = { ...defaultProps, item: { ...defaultProps.item, ...beast } };
        render(<Editor {...props} />);
        
        expect(screen.getByDisplayValue(beast.smarts)).toBeInTheDocument();
      });
    });
  });
});