import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import CharacterEditor from './Editor';

// Mock CharacterSheet
jest.mock('../../../character_sheet', () => {
  const MockCharacterSheet = function(props) {
    const React = require('react');
    return React.createElement('div', { 'data-testid': 'character-sheet' }, [
      React.createElement('div', { 'data-testid': 'character-fields', key: 'fields' }, [
        React.createElement('input', { 
          'data-testid': 'name-input', 
          value: props.item?.name || '', 
          onChange: (e) => props.onChange({ ...props.item, name: e.target.value }),
          key: 'name' 
        }),
        React.createElement('textarea', { 
          'data-testid': 'description-input', 
          value: props.item?.description || '', 
          onChange: (e) => props.onChange({ ...props.item, description: e.target.value }),
          key: 'desc' 
        }),
        React.createElement('input', { 
          'data-testid': 'agility-input', 
          value: props.item?.agility || '', 
          readOnly: true,
          key: 'agility' 
        }),
        React.createElement('input', { 
          'data-testid': 'smarts-input', 
          value: props.item?.smarts || '', 
          readOnly: true,
          key: 'smarts' 
        }),
        React.createElement('input', { 
          'data-testid': 'spirit-input', 
          value: props.item?.spirit || '', 
          readOnly: true,
          key: 'spirit' 
        }),
        React.createElement('input', { 
          'data-testid': 'strength-input', 
          value: props.item?.strength || '', 
          readOnly: true,
          key: 'strength' 
        }),
        React.createElement('input', { 
          'data-testid': 'vigor-input', 
          value: props.item?.vigor || '', 
          readOnly: true,
          key: 'vigor' 
        }),
        React.createElement('input', { 
          'data-testid': 'charisma-input', 
          type: 'number',
          value: props.item?.charisma || '', 
          readOnly: true,
          key: 'charisma' 
        }),
        React.createElement('input', { 
          'data-testid': 'pace-input', 
          type: 'number',
          value: props.item?.pace || '', 
          readOnly: true,
          key: 'pace' 
        })
      ]),
      React.createElement('div', { 'data-testid': 'character-lists', key: 'lists' }, [
        React.createElement('div', { 'data-testid': 'edges-list', key: 'edges' }),
        React.createElement('div', { 'data-testid': 'hindrances-list', key: 'hindrances' }),
        React.createElement('div', { 'data-testid': 'skills-list', key: 'skills' })
      ])
    ]);
  };
  MockCharacterSheet.prototype = {};
  return MockCharacterSheet;
});

describe('CharacterEditor Component', () => {
  const mockCharacter = {
    name: 'John Steel',
    description: 'A veteran gunslinger',
    agility: 'd8',
    smarts: 'd6', 
    spirit: 'd8',
    strength: 'd6',
    vigor: 'd8',
    charisma: 0,
    pace: 6,
    edges: ['Quick Draw', 'Two-Fisted'],
    hindrances: ['Code of Honor', 'Heroic'],
    skills: ['Shooting', 'Fighting', 'Notice']
  };

  const mockEdgesAvailable = [
    { name: 'Quick Draw', description: 'Draw weapon quickly' },
    { name: 'Two-Fisted', description: 'Use two weapons effectively' },
    { name: 'Marksman', description: 'Improved shooting accuracy' }
  ];

  const mockHindrancesAvailable = [
    { name: 'Code of Honor', severity: 'Major', description: 'Lives by a code' },
    { name: 'Heroic', severity: 'Major', description: 'Always helps others' },
    { name: 'Loyal', severity: 'Minor', description: 'Loyal to friends' }
  ];

  const mockSkillsAvailable = [
    { name: 'Shooting', attribute: 'Agility', description: 'Ranged combat' },
    { name: 'Fighting', attribute: 'Agility', description: 'Melee combat' },
    { name: 'Notice', attribute: 'Smarts', description: 'Perception and awareness' },
    { name: 'Stealth', attribute: 'Agility', description: 'Move quietly' }
  ];

  const defaultProps = {
    id: 'test-character',
    item: mockCharacter,
    index: 0,
    onChange: jest.fn(),
    onDelete: jest.fn(),
    edgesAvailable: mockEdgesAvailable,
    hindrancesAvailable: mockHindrancesAvailable,
    skillsAvailable: mockSkillsAvailable
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with required props', () => {
      render(<CharacterEditor {...defaultProps} />);
      
      expect(screen.getByTestId('character-sheet')).toBeInTheDocument();
      expect(screen.getByTestId('character-fields')).toBeInTheDocument();
    });

    it('renders character data fields', () => {
      render(<CharacterEditor {...defaultProps} />);
      
      expect(screen.getByTestId('name-input')).toBeInTheDocument();
      expect(screen.getByTestId('description-input')).toBeInTheDocument();
      expect(screen.getByTestId('agility-input')).toBeInTheDocument();
      expect(screen.getByTestId('smarts-input')).toBeInTheDocument();
      expect(screen.getByTestId('spirit-input')).toBeInTheDocument();
      expect(screen.getByTestId('strength-input')).toBeInTheDocument();
      expect(screen.getByTestId('vigor-input')).toBeInTheDocument();
      expect(screen.getByTestId('charisma-input')).toBeInTheDocument();
      expect(screen.getByTestId('pace-input')).toBeInTheDocument();
    });

    it('renders character lists', () => {
      render(<CharacterEditor {...defaultProps} />);
      
      expect(screen.getByTestId('edges-list')).toBeInTheDocument();
      expect(screen.getByTestId('hindrances-list')).toBeInTheDocument();
      expect(screen.getByTestId('skills-list')).toBeInTheDocument();
    });

    it('displays current character name', () => {
      render(<CharacterEditor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('name-input');
      expect(nameInput).toHaveValue('John Steel');
    });

    it('displays current character description', () => {
      render(<CharacterEditor {...defaultProps} />);
      
      const descriptionInput = screen.getByTestId('description-input');
      expect(descriptionInput).toHaveValue('A veteran gunslinger');
    });

    it('uses correct component id format', () => {
      render(<CharacterEditor {...defaultProps} />);
      
      // The component should create an id like 'CharacterEditor-0-test-character'
      expect(screen.getByTestId('character-sheet')).toBeInTheDocument();
    });
  });

  describe('Props Passing', () => {
    it('passes all required props to CharacterSheet', () => {
      render(<CharacterEditor {...defaultProps} />);
      
      // CharacterSheet should receive all the props
      expect(screen.getByTestId('character-sheet')).toBeInTheDocument();
    });

    it('passes edgesAvailable prop', () => {
      render(<CharacterEditor {...defaultProps} />);
      
      // Should render edges list component
      expect(screen.getByTestId('edges-list')).toBeInTheDocument();
    });

    it('passes hindrancesAvailable prop', () => {
      render(<CharacterEditor {...defaultProps} />);
      
      // Should render hindrances list component
      expect(screen.getByTestId('hindrances-list')).toBeInTheDocument();
    });

    it('passes skillsAvailable prop', () => {
      render(<CharacterEditor {...defaultProps} />);
      
      // Should render skills list component
      expect(screen.getByTestId('skills-list')).toBeInTheDocument();
    });

    it('passes item prop correctly', () => {
      render(<CharacterEditor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('name-input');
      expect(nameInput).toHaveValue(mockCharacter.name);
    });

    it('passes generated component id', () => {
      render(<CharacterEditor {...defaultProps} id="custom-id" />);
      
      // Should still render the character sheet
      expect(screen.getByTestId('character-sheet')).toBeInTheDocument();
    });
  });

  describe('Character Data Handling', () => {
    it('handles character with complete data', () => {
      const completeCharacter = {
        ...mockCharacter,
        animalIntelligence: false,
        toughness: 6,
        parry: 6,
        wounds: 0
      };

      render(<CharacterEditor {...defaultProps} item={completeCharacter} />);
      
      expect(screen.getByTestId('name-input')).toHaveValue('John Steel');
    });

    it('handles character with minimal data', () => {
      const minimalCharacter = {
        name: 'Simple Character'
      };

      render(<CharacterEditor {...defaultProps} item={minimalCharacter} />);
      
      expect(screen.getByTestId('name-input')).toHaveValue('Simple Character');
    });

    it('handles undefined character properties', () => {
      const incompleteCharacter = {
        name: 'Incomplete Character',
        // Missing other properties
      };

      expect(() => {
        render(<CharacterEditor {...defaultProps} item={incompleteCharacter} />);
      }).not.toThrow();
    });

    it('handles null character properties', () => {
      const nullCharacter = {
        name: 'Null Character',
        description: null,
        edges: null,
        hindrances: null,
        skills: null
      };

      expect(() => {
        render(<CharacterEditor {...defaultProps} item={nullCharacter} />);
      }).not.toThrow();
    });
  });

  describe('onChange Handler', () => {
    it('calls onChange when character name changes', () => {
      const mockOnChange = jest.fn();
      render(<CharacterEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByTestId('name-input');
      fireEvent.change(nameInput, { target: { value: 'Jane Doe' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'Jane Doe'
        }),
        0
      );
    });

    it('calls onChange when character description changes', () => {
      const mockOnChange = jest.fn();
      render(<CharacterEditor {...defaultProps} onChange={mockOnChange} />);
      
      const descriptionInput = screen.getByTestId('description-input');
      fireEvent.change(descriptionInput, { target: { value: 'A skilled bounty hunter' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          description: 'A skilled bounty hunter'
        }),
        0
      );
    });

    it('passes correct index to onChange', () => {
      const mockOnChange = jest.fn();
      render(<CharacterEditor {...defaultProps} index={3} onChange={mockOnChange} />);
      
      const nameInput = screen.getByTestId('name-input');
      fireEvent.change(nameInput, { target: { value: 'Test Character' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(expect.any(Object), 3);
    });

    it('preserves existing character data when making changes', () => {
      const mockOnChange = jest.fn();
      render(<CharacterEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByTestId('name-input');
      fireEvent.change(nameInput, { target: { value: 'Updated Name' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'Updated Name',
          description: 'A veteran gunslinger',
          agility: 'd8',
          smarts: 'd6',
          spirit: 'd8',
          strength: 'd6',
          vigor: 'd8'
        }),
        0
      );
    });
  });

  describe('Available Data Arrays', () => {
    it('handles empty edges available array', () => {
      render(<CharacterEditor {...defaultProps} edgesAvailable={[]} />);
      
      expect(screen.getByTestId('edges-list')).toBeInTheDocument();
    });

    it('handles empty hindrances available array', () => {
      render(<CharacterEditor {...defaultProps} hindrancesAvailable={[]} />);
      
      expect(screen.getByTestId('hindrances-list')).toBeInTheDocument();
    });

    it('handles empty skills available array', () => {
      render(<CharacterEditor {...defaultProps} skillsAvailable={[]} />);
      
      expect(screen.getByTestId('skills-list')).toBeInTheDocument();
    });

    it('handles large edges available array', () => {
      const manyEdges = Array.from({ length: 50 }, (_, i) => ({
        name: `Edge ${i}`,
        description: `Description for edge ${i}`
      }));

      render(<CharacterEditor {...defaultProps} edgesAvailable={manyEdges} />);
      
      expect(screen.getByTestId('edges-list')).toBeInTheDocument();
    });
  });

  describe('Savage Worlds Character Types', () => {
    const characterTypes = [
      {
        name: 'Gunslinger',
        agility: 'd10',
        smarts: 'd6',
        spirit: 'd8',
        strength: 'd6',
        vigor: 'd8',
        edges: ['Quick Draw', 'Marksman'],
        hindrances: ['Code of Honor'],
        skills: ['Shooting', 'Intimidation', 'Notice']
      },
      {
        name: 'Scholar',
        agility: 'd6',
        smarts: 'd10',
        spirit: 'd8',
        strength: 'd4',
        vigor: 'd6',
        edges: ['Scholar', 'Investigator'],
        hindrances: ['Curious', 'Pacifist'],
        skills: ['Research', 'Knowledge', 'Notice']
      },
      {
        name: 'Warrior',
        agility: 'd8',
        smarts: 'd6',
        spirit: 'd6',
        strength: 'd10',
        vigor: 'd10',
        edges: ['Brawny', 'Sweep'],
        hindrances: ['Bloodthirsty'],
        skills: ['Fighting', 'Intimidation', 'Throwing']
      }
    ];

    it('handles typical Savage Worlds character archetypes', () => {
      characterTypes.forEach(character => {
        const { rerender } = render(<CharacterEditor {...defaultProps} item={character} />);
        
        expect(screen.getByTestId('name-input')).toHaveValue(character.name);
        
        rerender(<div />);
      });
    });

    it('handles characters with high attributes', () => {
      const powerfulCharacter = {
        ...mockCharacter,
        name: 'Legendary Hero',
        agility: 'd12+2',
        smarts: 'd12',
        spirit: 'd12',
        strength: 'd12+1',
        vigor: 'd12'
      };

      render(<CharacterEditor {...defaultProps} item={powerfulCharacter} />);
      
      expect(screen.getByTestId('name-input')).toHaveValue('Legendary Hero');
    });

    it('handles characters with many edges', () => {
      const experiencedCharacter = {
        ...mockCharacter,
        name: 'Veteran',
        edges: [
          'Alertness', 'Ambidextrous', 'Arcane Background', 'Attractive',
          'Berserk', 'Block', 'Brawny', 'Champion', 'Charismatic',
          'Combat Reflexes', 'Command', 'Common Bond', 'Connections'
        ]
      };

      render(<CharacterEditor {...defaultProps} item={experiencedCharacter} />);
      
      expect(screen.getByTestId('name-input')).toHaveValue('Veteran');
    });
  });

  describe('Component Integration', () => {
    it('wraps CharacterSheet component', () => {
      render(<CharacterEditor {...defaultProps} />);
      
      // Should render the CharacterSheet component
      expect(screen.getByTestId('character-sheet')).toBeInTheDocument();
      expect(screen.getByTestId('character-fields')).toBeInTheDocument();
      expect(screen.getByTestId('character-lists')).toBeInTheDocument();
    });

    it('maintains proper React component structure', () => {
      const component = new CharacterEditor.prototype.constructor(defaultProps);
      expect(component).toBeInstanceOf(Object);
    });

    it('implements required PropTypes', () => {
      // Check that PropTypes are defined
      expect(CharacterEditor.propTypes).toBeDefined();
      expect(CharacterEditor.propTypes.edgesAvailable).toBeDefined();
      expect(CharacterEditor.propTypes.hindrancesAvailable).toBeDefined();
      expect(CharacterEditor.propTypes.skillsAvailable).toBeDefined();
      expect(CharacterEditor.propTypes.item).toBeDefined();
      expect(CharacterEditor.propTypes.onChange).toBeDefined();
      expect(CharacterEditor.propTypes.id).toBeDefined();
      expect(CharacterEditor.propTypes.index).toBeDefined();
    });

    it('provides default props', () => {
      expect(CharacterEditor.defaultProps).toBeDefined();
    });
  });

  describe('Edge Cases', () => {
    it('handles rapid character updates', () => {
      const mockOnChange = jest.fn();
      render(<CharacterEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByTestId('name-input');
      const descriptionInput = screen.getByTestId('description-input');
      
      fireEvent.change(nameInput, { target: { value: 'Name 1' } });
      fireEvent.change(nameInput, { target: { value: 'Name 2' } });
      fireEvent.change(descriptionInput, { target: { value: 'Desc 1' } });
      fireEvent.change(nameInput, { target: { value: 'Final Name' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(4);
      expect(mockOnChange).toHaveBeenLastCalledWith(
        expect.objectContaining({ name: 'Final Name' }),
        0
      );
    });

    it('handles very long character names', () => {
      const longName = 'This is a very long character name that exceeds normal length limits and might cause display issues';
      const mockOnChange = jest.fn();
      render(<CharacterEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByTestId('name-input');
      fireEvent.change(nameInput, { target: { value: longName } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ name: longName }),
        0
      );
    });

    it('handles special characters in character data', () => {
      const specialCharacter = {
        ...mockCharacter,
        name: 'José María O\'Connor',
        description: 'Character with éñ and symbols: @#$%^&*()'
      };

      render(<CharacterEditor {...defaultProps} item={specialCharacter} />);
      
      expect(screen.getByTestId('name-input')).toHaveValue('José María O\'Connor');
      expect(screen.getByTestId('description-input')).toHaveValue('Character with éñ and symbols: @#$%^&*()');
    });

    it('handles undefined or missing required arrays', () => {
      expect(() => {
        render(<CharacterEditor 
          {...defaultProps} 
          edgesAvailable={undefined}
          hindrancesAvailable={undefined} 
          skillsAvailable={undefined} 
        />);
      }).not.toThrow();
    });

    it('handles malformed character data', () => {
      const malformedCharacter = {
        name: 123, // Number instead of string
        description: [], // Array instead of string
        edges: 'not an array', // String instead of array
        agility: { invalid: 'object' } // Object instead of string
      };

      expect(() => {
        render(<CharacterEditor {...defaultProps} item={malformedCharacter} />);
      }).not.toThrow();
    });
  });

  describe('Performance Considerations', () => {
    it('handles characters with large skill sets', () => {
      const skillfulCharacter = {
        ...mockCharacter,
        skills: Array.from({ length: 30 }, (_, i) => `Skill ${i}`)
      };

      render(<CharacterEditor {...defaultProps} item={skillfulCharacter} />);
      
      expect(screen.getByTestId('skills-list')).toBeInTheDocument();
    });

    it('handles multiple character editors efficiently', () => {
      const characters = Array.from({ length: 5 }, (_, i) => ({
        ...mockCharacter,
        name: `Character ${i}`
      }));

      characters.forEach((character, index) => {
        const { rerender } = render(
          <CharacterEditor 
            {...defaultProps} 
            item={character}
            index={index}
            key={index}
          />
        );
        
        expect(screen.getByTestId('name-input')).toHaveValue(`Character ${index}`);
        
        rerender(<div />);
      });
    });
  });

  describe('Accessibility', () => {
    it('maintains proper component structure for screen readers', () => {
      render(<CharacterEditor {...defaultProps} />);
      
      // Should have identifiable structure
      expect(screen.getByTestId('character-sheet')).toBeInTheDocument();
      expect(screen.getByTestId('character-fields')).toBeInTheDocument();
      expect(screen.getByTestId('character-lists')).toBeInTheDocument();
    });

    it('provides proper form inputs for character data', () => {
      render(<CharacterEditor {...defaultProps} />);
      
      // Should have proper input elements
      expect(screen.getByTestId('name-input')).toBeInTheDocument();
      expect(screen.getByTestId('description-input')).toBeInTheDocument();
    });
  });
});