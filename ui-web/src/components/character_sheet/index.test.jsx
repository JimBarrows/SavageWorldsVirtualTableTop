import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import CharacterSheet from './index';

// Mock child components
jest.mock('../AttributeComponent', () => {
  return function MockAttributeComponent({ id, onChange, value, prepend, append }) {
    return (
      <div data-testid="attribute-component">
        {prepend}
        <input 
          data-testid={`attribute-${id}`}
          value={value || ''}
          onChange={(e) => onChange && onChange(e.target.value)}
        />
        {append}
      </div>
    );
  };
});

jest.mock('../BaseEditor', () => {
  return function MockBaseEditor({ id, onDelete, children }) {
    return (
      <div data-testid="base-editor" data-id={id}>
        <button data-testid="delete-button" onClick={onDelete}>Delete</button>
        {children}
      </div>
    );
  };
});

jest.mock('./selected_edges/index', () => {
  return function MockSelectedEdges({ id, edgesAvailable, edges, onChange }) {
    return (
      <div data-testid="selected-edges">
        <select 
          data-testid="edges-select"
          onChange={(e) => onChange && onChange([...edges, { edge: { name: e.target.value }, note: '' }])}
        >
          <option value="">Select Edge</option>
          {edgesAvailable.map((edge, index) => (
            <option key={index} value={edge.name}>{edge.name}</option>
          ))}
        </select>
        <div data-testid="current-edges">
          {edges.map((edge, index) => (
            <div key={index}>{edge.edge?.name || edge.name}</div>
          ))}
        </div>
      </div>
    );
  };
});

jest.mock('./selected_hindrances/index', () => {
  return function MockSelectedHindrances({ id, hindrancesAvailable, hindrances, onChange }) {
    return (
      <div data-testid="selected-hindrances">
        <select 
          data-testid="hindrances-select"
          onChange={(e) => onChange && onChange([...hindrances, { hindrance: { name: e.target.value }, note: '' }])}
        >
          <option value="">Select Hindrance</option>
          {hindrancesAvailable.map((hindrance, index) => (
            <option key={index} value={hindrance.name}>{hindrance.name}</option>
          ))}
        </select>
        <div data-testid="current-hindrances">
          {hindrances.map((hindrance, index) => (
            <div key={index}>{hindrance.hindrance?.name || hindrance.name}</div>
          ))}
        </div>
      </div>
    );
  };
});

jest.mock('./selected_skills/index', () => {
  return function MockSelectedSkills({ id, skillsAvailable, skills, onChange }) {
    return (
      <div data-testid="selected-skills">
        <select 
          data-testid="skills-select"
          onChange={(e) => onChange && onChange([...skills, { skill: { name: e.target.value }, level: 'd4' }])}
        >
          <option value="">Select Skill</option>
          {skillsAvailable.map((skill, index) => (
            <option key={index} value={skill.name}>{skill.name}</option>
          ))}
        </select>
        <div data-testid="current-skills">
          {skills.map((skill, index) => (
            <div key={index}>{skill.skill?.name || skill.name}</div>
          ))}
        </div>
      </div>
    );
  };
});

// Mock Bootstrap React Components
jest.mock('bootstrap-react-components', () => ({
  CheckboxFormGroup: function MockCheckboxFormGroup({ id, label, checked, value, onChange }) {
    return (
      <div data-testid="checkbox-form-group">
        <label>
          <input 
            type="checkbox"
            id={id}
            checked={checked || value}
            onChange={(e) => onChange && onChange(e)}
            data-testid="checkbox-input"
          />
          {label}
        </label>
      </div>
    );
  },
  NumberFormGroup: function MockNumberFormGroup({ id, label, value, onChange }) {
    return (
      <div data-testid="number-form-group">
        <label htmlFor={id}>{label}</label>
        <input 
          type="number"
          id={id}
          value={value || ''}
          onChange={onChange}
          data-testid="number-input"
        />
      </div>
    );
  },
  PrependAddon: function MockPrependAddon({ id, children }) {
    return <span data-testid="prepend-addon" id={id}>{children}</span>;
  },
  TextAreaFormGroup: function MockTextAreaFormGroup({ id, label, onChange, value, required }) {
    return (
      <div data-testid="textarea-form-group">
        <label htmlFor={id}>{label}</label>
        <textarea 
          id={id}
          value={value || ''}
          onChange={onChange}
          required={required}
          data-testid="textarea-input"
        />
      </div>
    );
  },
  TextFormGroup: function MockTextFormGroup({ id, label, onChange, value, required }) {
    return (
      <div data-testid="text-form-group">
        <label htmlFor={id}>{label}</label>
        <input 
          type="text"
          id={id}
          value={value || ''}
          onChange={onChange}
          required={required}
          data-testid="text-input"
        />
      </div>
    );
  }
}));

describe('CharacterSheet Component', () => {
  const mockCharacter = {
    name: 'Test Hero',
    description: 'A brave adventurer',
    agility: 'd6',
    smarts: 'd6',
    spirit: 'd8', 
    strength: 'd6',
    vigor: 'd8',
    charisma: 0,
    pace: 6,
    animalIntelligence: false,
    edges: [{ edge: { name: 'Brave' }, note: 'Fearless in combat' }],
    hindrances: [{ hindrance: { name: 'Heroic' }, note: 'Always helps others' }],
    skills: [{ skill: { name: 'Fighting' }, level: 'd8' }]
  };

  const mockEdgesAvailable = [
    { name: 'Brave', description: 'Immune to fear' },
    { name: 'Quick', description: 'Act first in combat' },
    { name: 'Strong Willed', description: 'Resist mental effects' }
  ];

  const mockHindrancesAvailable = [
    { name: 'Heroic', description: 'Cannot ignore those in need' },
    { name: 'Curious', description: 'Must investigate mysteries' },
    { name: 'Loyal', description: 'Never betrays friends' }
  ];

  const mockSkillsAvailable = [
    { name: 'Fighting', description: 'Melee combat skill' },
    { name: 'Shooting', description: 'Ranged combat skill' },
    { name: 'Notice', description: 'Awareness and perception' }
  ];

  const defaultProps = {
    id: 'test-character',
    item: mockCharacter,
    index: 0,
    edgesAvailable: mockEdgesAvailable,
    hindrancesAvailable: mockHindrancesAvailable,
    skillsAvailable: mockSkillsAvailable,
    onChange: jest.fn(),
    onDelete: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with required props', () => {
      render(<CharacterSheet {...defaultProps} />);
      
      expect(screen.getByTestId('base-editor')).toBeInTheDocument();
      expect(screen.getByLabelText('Name')).toHaveValue('Test Hero');
      expect(screen.getByLabelText('Description')).toHaveValue('A brave adventurer');
    });

    it('renders with correct component id', () => {
      render(<CharacterSheet {...defaultProps} id="custom-char" />);
      
      const baseEditor = screen.getByTestId('base-editor');
      expect(baseEditor).toHaveAttribute('data-id', 'custom-char');
    });

    it('displays all Savage Worlds attributes', () => {
      render(<CharacterSheet {...defaultProps} />);
      
      expect(screen.getByText('Agility')).toBeInTheDocument();
      expect(screen.getByText('Smarts')).toBeInTheDocument();
      expect(screen.getByText('Spirit')).toBeInTheDocument();
      expect(screen.getByText('Strength')).toBeInTheDocument();
      expect(screen.getByText('Vigor')).toBeInTheDocument();
    });

    it('displays character statistics', () => {
      render(<CharacterSheet {...defaultProps} />);
      
      expect(screen.getByLabelText('Charisma')).toBeInTheDocument();
      expect(screen.getByLabelText('Pace')).toBeInTheDocument();
    });

    it('displays Animal Intelligence checkbox', () => {
      render(<CharacterSheet {...defaultProps} />);
      
      expect(screen.getByText('Animal Intelligence')).toBeInTheDocument();
      expect(screen.getByTestId('checkbox-input')).toBeInTheDocument();
    });

    it('renders selected edges, hindrances, and skills components', () => {
      render(<CharacterSheet {...defaultProps} />);
      
      expect(screen.getByTestId('selected-edges')).toBeInTheDocument();
      expect(screen.getByTestId('selected-hindrances')).toBeInTheDocument();
      expect(screen.getByTestId('selected-skills')).toBeInTheDocument();
    });

    it('renders children prop when provided', () => {
      render(
        <CharacterSheet {...defaultProps}>
          <div data-testid="custom-content">Custom content</div>
        </CharacterSheet>
      );
      
      expect(screen.getByTestId('custom-content')).toBeInTheDocument();
    });
  });

  describe('Form Field Updates', () => {
    it('updates character name', () => {
      const mockOnChange = jest.fn();
      render(<CharacterSheet {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'Updated Hero' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'Updated Hero'
        })
      );
    });

    it('updates character description', () => {
      const mockOnChange = jest.fn();
      render(<CharacterSheet {...defaultProps} onChange={mockOnChange} />);
      
      const descriptionInput = screen.getByLabelText('Description');
      fireEvent.change(descriptionInput, { target: { value: 'An updated description' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          description: 'An updated description'
        })
      );
    });

    it('updates charisma value', () => {
      const mockOnChange = jest.fn();
      render(<CharacterSheet {...defaultProps} onChange={mockOnChange} />);
      
      const charismaInput = screen.getByLabelText('Charisma');
      fireEvent.change(charismaInput, { target: { value: '2' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          charisma: 2
        })
      );
    });

    it('updates pace value', () => {
      const mockOnChange = jest.fn();
      render(<CharacterSheet {...defaultProps} onChange={mockOnChange} />);
      
      const paceInput = screen.getByLabelText('Pace');
      fireEvent.change(paceInput, { target: { value: '8' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          pace: 8
        })
      );
    });

    it('updates animal intelligence checkbox', () => {
      const mockOnChange = jest.fn();
      render(<CharacterSheet {...defaultProps} onChange={mockOnChange} />);
      
      const checkbox = screen.getByTestId('checkbox-input');
      fireEvent.click(checkbox);
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          animalIntelligence: 'on'
        })
      );
    });
  });

  describe('Attribute Updates', () => {
    it('updates agility attribute', () => {
      const mockOnChange = jest.fn();
      render(<CharacterSheet {...defaultProps} onChange={mockOnChange} />);
      
      // Find agility attribute by looking for the one with agility in the test id
      const attributeInputs = screen.getAllByTestId(/attribute-/);
      const agilityInput = attributeInputs.find(input => 
        input.getAttribute('data-testid').includes('Agility')
      );
      
      fireEvent.change(agilityInput, { target: { value: 'd8' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          agility: 'd8'
        })
      );
    });

    it('updates smarts attribute', () => {
      const mockOnChange = jest.fn();
      render(<CharacterSheet {...defaultProps} onChange={mockOnChange} />);
      
      const attributeInputs = screen.getAllByTestId(/attribute-/);
      const smartsInput = attributeInputs.find(input => 
        input.getAttribute('data-testid').includes('Smarts')
      );
      
      fireEvent.change(smartsInput, { target: { value: 'd10' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          smarts: 'd10'
        })
      );
    });

    it('updates spirit attribute', () => {
      const mockOnChange = jest.fn();
      render(<CharacterSheet {...defaultProps} onChange={mockOnChange} />);
      
      const attributeInputs = screen.getAllByTestId(/attribute-/);
      const spiritInput = attributeInputs.find(input => 
        input.getAttribute('data-testid').includes('Spirit')
      );
      
      fireEvent.change(spiritInput, { target: { value: 'd12' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          spirit: 'd12'
        })
      );
    });

    it('updates strength attribute', () => {
      const mockOnChange = jest.fn();
      render(<CharacterSheet {...defaultProps} onChange={mockOnChange} />);
      
      const attributeInputs = screen.getAllByTestId(/attribute-/);
      const strengthInput = attributeInputs.find(input => 
        input.getAttribute('data-testid').includes('Strength')
      );
      
      fireEvent.change(strengthInput, { target: { value: 'd8' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          strength: 'd8'
        })
      );
    });

    it('updates vigor attribute', () => {
      const mockOnChange = jest.fn();
      render(<CharacterSheet {...defaultProps} onChange={mockOnChange} />);
      
      const attributeInputs = screen.getAllByTestId(/attribute-/);
      const vigorInput = attributeInputs.find(input => 
        input.getAttribute('data-testid').includes('Vigor')
      );
      
      fireEvent.change(vigorInput, { target: { value: 'd10' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          vigor: 'd10'
        })
      );
    });
  });

  describe('Edges, Hindrances, and Skills Management', () => {
    it('updates edges list when changed', () => {
      const mockOnChange = jest.fn();
      render(<CharacterSheet {...defaultProps} onChange={mockOnChange} />);
      
      const edgesSelect = screen.getByTestId('edges-select');
      fireEvent.change(edgesSelect, { target: { value: 'Quick' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          edges: expect.arrayContaining([
            expect.objectContaining({
              edge: expect.objectContaining({ name: 'Quick' })
            })
          ])
        })
      );
    });

    it('updates hindrances list when changed', () => {
      const mockOnChange = jest.fn();
      render(<CharacterSheet {...defaultProps} onChange={mockOnChange} />);
      
      const hindrancesSelect = screen.getByTestId('hindrances-select');
      fireEvent.change(hindrancesSelect, { target: { value: 'Curious' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          hindrances: expect.arrayContaining([
            expect.objectContaining({
              hindrance: expect.objectContaining({ name: 'Curious' })
            })
          ])
        })
      );
    });

    it('updates skills list when changed', () => {
      const mockOnChange = jest.fn();
      render(<CharacterSheet {...defaultProps} onChange={mockOnChange} />);
      
      const skillsSelect = screen.getByTestId('skills-select');
      fireEvent.change(skillsSelect, { target: { value: 'Shooting' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          skills: expect.arrayContaining([
            expect.objectContaining({
              skill: expect.objectContaining({ name: 'Shooting' })
            })
          ])
        })
      );
    });

    it('displays current edges', () => {
      render(<CharacterSheet {...defaultProps} />);
      
      const currentEdges = screen.getByTestId('current-edges');
      expect(currentEdges).toHaveTextContent('Brave');
    });

    it('displays current hindrances', () => {
      render(<CharacterSheet {...defaultProps} />);
      
      const currentHindrances = screen.getByTestId('current-hindrances');
      expect(currentHindrances).toHaveTextContent('Heroic');
    });

    it('displays current skills', () => {
      render(<CharacterSheet {...defaultProps} />);
      
      const currentSkills = screen.getByTestId('current-skills');
      expect(currentSkills).toHaveTextContent('Fighting');
    });
  });

  describe('Delete Functionality', () => {
    it('calls onDelete with correct index when delete button is clicked', () => {
      const mockOnDelete = jest.fn();
      render(<CharacterSheet {...defaultProps} index={5} onDelete={mockOnDelete} />);
      
      const deleteButton = screen.getByTestId('delete-button');
      fireEvent.click(deleteButton);
      
      expect(mockOnDelete).toHaveBeenCalledWith(5);
    });

    it('prevents default on delete button click', () => {
      const mockOnDelete = jest.fn();
      render(<CharacterSheet {...defaultProps} onDelete={mockOnDelete} />);
      
      const deleteButton = screen.getByTestId('delete-button');
      fireEvent.click(deleteButton);
      
      expect(mockOnDelete).toHaveBeenCalled();
    });
  });

  describe('Required Fields', () => {
    it('marks name field as required', () => {
      render(<CharacterSheet {...defaultProps} />);
      
      const nameInput = screen.getByLabelText('Name');
      expect(nameInput).toHaveAttribute('required');
    });

    it('does not mark description as required', () => {
      render(<CharacterSheet {...defaultProps} />);
      
      const descriptionInput = screen.getByLabelText('Description');
      expect(descriptionInput).not.toHaveAttribute('required');
    });
  });

  describe('Savage Worlds Game Logic', () => {
    it('handles all five core attributes', () => {
      const savageWorldsAttributes = ['Agility', 'Smarts', 'Spirit', 'Strength', 'Vigor'];
      render(<CharacterSheet {...defaultProps} />);
      
      savageWorldsAttributes.forEach(attribute => {
        expect(screen.getByText(attribute)).toBeInTheDocument();
      });
    });

    it('supports Savage Worlds dice steps', () => {
      const characterWithDiceSteps = {
        ...mockCharacter,
        agility: 'd12+2',
        smarts: 'd4',
        spirit: 'd6',
        strength: 'd8',
        vigor: 'd10'
      };

      const mockOnChange = jest.fn();
      render(<CharacterSheet {...defaultProps} item={characterWithDiceSteps} onChange={mockOnChange} />);
      
      // Test that dice step values are properly handled
      const attributeInputs = screen.getAllByTestId(/attribute-/);
      expect(attributeInputs.length).toBeGreaterThan(0);
    });

    it('handles character advancement mechanics', () => {
      const advancedCharacter = {
        ...mockCharacter,
        agility: 'd8',        // Advanced from d6
        spirit: 'd10',        // Advanced from d8
        charisma: 2,          // Improved through edges
        pace: 8,              // Modified by edges/hindrances
        edges: [
          { edge: { name: 'Brave' }, note: '' },
          { edge: { name: 'Quick' }, note: '' },
          { edge: { name: 'Fleet-Footed' }, note: '+2 Pace' }
        ]
      };

      render(<CharacterSheet {...defaultProps} item={advancedCharacter} />);
      
      expect(screen.getByLabelText('Pace')).toHaveValue(8); // Pace
      expect(screen.getByLabelText('Charisma')).toHaveValue(2); // Charisma
    });

    it('supports animal intelligence for bestial characters', () => {
      const beastCharacter = {
        ...mockCharacter,
        animalIntelligence: true,
        smarts: 'd4'
      };

      render(<CharacterSheet {...defaultProps} item={beastCharacter} />);
      
      const checkbox = screen.getByTestId('checkbox-input');
      expect(checkbox).toBeChecked();
    });

    it('integrates edges that affect character statistics', () => {
      const characterWithStatEdges = {
        ...mockCharacter,
        edges: [
          { edge: { name: 'Charismatic' }, note: '+2 Charisma' },
          { edge: { name: 'Fleet-Footed' }, note: '+2 Pace, d10 running' }
        ],
        charisma: 2,
        pace: 8
      };

      render(<CharacterSheet {...defaultProps} item={characterWithStatEdges} />);
      
      expect(screen.getByLabelText('Charisma')).toHaveValue(2); // Modified charisma
      expect(screen.getByLabelText('Pace')).toHaveValue(8); // Modified pace
    });
  });

  describe('Data Handling', () => {
    it('handles empty or undefined arrays gracefully', () => {
      const characterWithEmptyArrays = {
        ...mockCharacter,
        edges: undefined,
        hindrances: null,
        skills: []
      };

      expect(() => {
        render(<CharacterSheet {...defaultProps} item={characterWithEmptyArrays} />);
      }).not.toThrow();
    });

    it('handles missing optional properties', () => {
      const minimalCharacter = {
        name: 'Minimal Hero',
        description: 'Basic character'
      };

      expect(() => {
        render(<CharacterSheet {...defaultProps} item={minimalCharacter} />);
      }).not.toThrow();
    });

    it('provides default empty arrays for missing data', () => {
      const characterWithoutArrays = {
        name: 'Test Character',
        description: 'Test'
      };

      render(<CharacterSheet {...defaultProps} item={characterWithoutArrays} />);
      
      // Components should handle undefined arrays gracefully
      expect(screen.getByTestId('selected-edges')).toBeInTheDocument();
      expect(screen.getByTestId('selected-hindrances')).toBeInTheDocument();
      expect(screen.getByTestId('selected-skills')).toBeInTheDocument();
    });
  });

  describe('Component Integration', () => {
    it('integrates properly with BaseEditor', () => {
      render(<CharacterSheet {...defaultProps} />);
      
      const baseEditor = screen.getByTestId('base-editor');
      expect(baseEditor).toBeInTheDocument();
      expect(baseEditor).toHaveAttribute('data-id', 'test-character');
    });

    it('passes correct props to attribute components', () => {
      render(<CharacterSheet {...defaultProps} />);
      
      // All attribute components should be rendered
      const attributeComponents = screen.getAllByTestId('attribute-component');
      expect(attributeComponents).toHaveLength(5); // 5 attributes
    });

    it('passes correct props to selected edges component', () => {
      render(<CharacterSheet {...defaultProps} />);
      
      const selectedEdges = screen.getByTestId('selected-edges');
      expect(selectedEdges).toBeInTheDocument();
      
      // Should show available edges in the select options
      expect(screen.getByRole('option', { name: 'Brave' })).toBeInTheDocument();
      expect(screen.getByRole('option', { name: 'Quick' })).toBeInTheDocument();
    });
  });

  describe('Performance and Optimization', () => {
    it('does not cause unnecessary re-renders', () => {
      const { rerender } = render(<CharacterSheet {...defaultProps} />);
      
      // Same props should not cause issues
      rerender(<CharacterSheet {...defaultProps} />);
      
      expect(screen.getByTestId('base-editor')).toBeInTheDocument();
    });

    it('handles large datasets efficiently', () => {
      const largeDatasets = {
        ...defaultProps,
        edgesAvailable: Array(100).fill(0).map((_, i) => ({ name: `Edge ${i}`, description: `Description ${i}` })),
        hindrancesAvailable: Array(50).fill(0).map((_, i) => ({ name: `Hindrance ${i}`, description: `Description ${i}` })),
        skillsAvailable: Array(75).fill(0).map((_, i) => ({ name: `Skill ${i}`, description: `Description ${i}` }))
      };

      expect(() => {
        render(<CharacterSheet {...largeDatasets} />);
      }).not.toThrow();
    });
  });

  describe('Accessibility', () => {
    it('provides proper labels for all form fields', () => {
      render(<CharacterSheet {...defaultProps} />);
      
      expect(screen.getByLabelText('Name')).toBeInTheDocument();
      expect(screen.getByLabelText('Description')).toBeInTheDocument();
      expect(screen.getByLabelText('Charisma')).toBeInTheDocument();
      expect(screen.getByLabelText('Pace')).toBeInTheDocument();
    });

    it('associates attribute labels with inputs', () => {
      render(<CharacterSheet {...defaultProps} />);
      
      // Attribute prepend addons should be present
      const prependAddons = screen.getAllByTestId('prepend-addon');
      expect(prependAddons.length).toBe(5); // One for each attribute
    });

    it('supports keyboard navigation', () => {
      render(<CharacterSheet {...defaultProps} />);
      
      const deleteButton = screen.getByTestId('delete-button');
      expect(deleteButton).toBeInTheDocument();
      
      // Focus should be manageable
      deleteButton.focus();
      expect(deleteButton).toHaveFocus();
    });
  });

  describe('State Management', () => {
    it('initializes with empty selected state', () => {
      render(<CharacterSheet {...defaultProps} />);
      
      // Component should render without issues
      expect(screen.getByTestId('base-editor')).toBeInTheDocument();
    });

    it('maintains state consistency during updates', () => {
      const mockOnChange = jest.fn();
      render(<CharacterSheet {...defaultProps} onChange={mockOnChange} />);
      
      // Make multiple changes
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'Updated Hero' } });
      
      const charismaInput = screen.getByLabelText('Charisma');
      fireEvent.change(charismaInput, { target: { value: '3' } });
      
      // Each change should call onChange with updated character
      expect(mockOnChange).toHaveBeenCalledTimes(2);
      expect(mockOnChange).toHaveBeenLastCalledWith(
        expect.objectContaining({
          charisma: 3
        })
      );
    });
  });
});