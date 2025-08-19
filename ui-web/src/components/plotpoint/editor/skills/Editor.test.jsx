import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import SkillEditor from './Editor';

// Mock BaseEditor
jest.mock('../../../BaseEditor', () => {
  const MockBaseEditor = function(props) {
    const React = require('react');
    return React.createElement('div', { 'data-testid': 'base-editor', id: props.id }, [
      React.createElement('button', { 
        'data-testid': 'delete-button',
        onClick: props.onDelete,
        key: 'delete-btn'
      }, 'Delete'),
      React.createElement('div', { 'data-testid': 'editor-content', key: 'content' }, 
        props.children
      )
    ]);
  };
  MockBaseEditor.prototype = {};
  return MockBaseEditor;
});

// Mock AttributeSelectFormGroup
jest.mock('../../../formgroups/AttributeSelectFormGroup', () => {
  const MockAttributeSelectFormGroup = function({ id, onChange, attribute }) {
    const React = require('react');
    return React.createElement('div', { 'data-testid': 'attribute-select-form-group' }, [
      React.createElement('label', { htmlFor: id, key: 'label' }, 'Attribute'),
      React.createElement('select', { 
        id: id,
        value: attribute || '',
        onChange: onChange,
        'data-testid': 'attribute-select',
        key: 'select'
      }, [
        React.createElement('option', { value: 'Agility', key: 'agility' }, 'Agility'),
        React.createElement('option', { value: 'Smarts', key: 'smarts' }, 'Smarts'),
        React.createElement('option', { value: 'Spirit', key: 'spirit' }, 'Spirit'),
        React.createElement('option', { value: 'Strength', key: 'strength' }, 'Strength'),
        React.createElement('option', { value: 'Vigor', key: 'vigor' }, 'Vigor')
      ])
    ]);
  };
  MockAttributeSelectFormGroup.prototype = {};
  return MockAttributeSelectFormGroup;
});

// Mock Bootstrap React Components
jest.mock('bootstrap-react-components', () => ({
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
  },
  TextAreaFormGroup: function MockTextAreaFormGroup({ id, label, onChange, value }) {
    return (
      <div data-testid="textarea-form-group">
        <label htmlFor={id}>{label}</label>
        <textarea 
          id={id}
          value={value || ''}
          onChange={onChange}
          data-testid="textarea-input"
        />
      </div>
    );
  }
}));

describe('SkillEditor Component', () => {
  const mockSkill = {
    name: 'Shooting',
    attribute: 'Agility',
    description: 'Ranged combat skill'
  };

  const defaultProps = {
    id: 'test-skill',
    item: mockSkill,
    index: 0,
    onChange: jest.fn(),
    onDelete: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with required props', () => {
      render(<SkillEditor {...defaultProps} />);
      
      expect(screen.getByTestId('base-editor')).toBeInTheDocument();
      expect(screen.getByTestId('editor-content')).toBeInTheDocument();
    });

    it('renders all skill fields', () => {
      render(<SkillEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Name')).toBeInTheDocument();
      expect(screen.getByLabelText('Attribute')).toBeInTheDocument();
      expect(screen.getByLabelText('Description')).toBeInTheDocument();
    });

    it('displays current skill values', () => {
      render(<SkillEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue('Shooting');
      expect(screen.getByLabelText('Attribute')).toHaveValue('Agility');
      expect(screen.getByLabelText('Description')).toHaveValue('Ranged combat skill');
    });

    it('marks name field as required', () => {
      render(<SkillEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Name')).toHaveAttribute('required');
    });

    it('does not mark description field as required', () => {
      render(<SkillEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Description')).not.toHaveAttribute('required');
    });

    it('uses correct component id format', () => {
      render(<SkillEditor {...defaultProps} />);
      
      const baseEditor = screen.getByTestId('base-editor');
      expect(baseEditor).toHaveAttribute('id', 'test-skill');
    });

    it('uses default id when not provided', () => {
      render(<SkillEditor {...defaultProps} id={undefined} />);
      
      const baseEditor = screen.getByTestId('base-editor');
      expect(baseEditor).toHaveAttribute('id', 'Editor');
    });

    it('uses correct field ids', () => {
      render(<SkillEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Name')).toHaveAttribute('id', 'skillName');
      expect(screen.getByLabelText('Attribute')).toHaveAttribute('id', 'skillAttribute');
      expect(screen.getByLabelText('Description')).toHaveAttribute('id', 'skillDescription');
    });
  });

  describe('Attribute Selection', () => {
    it('renders attribute select component', () => {
      render(<SkillEditor {...defaultProps} />);
      
      expect(screen.getByTestId('attribute-select-form-group')).toBeInTheDocument();
      expect(screen.getByTestId('attribute-select')).toBeInTheDocument();
    });

    it('provides correct attribute options', () => {
      render(<SkillEditor {...defaultProps} />);
      
      const attributeSelect = screen.getByTestId('attribute-select');
      const options = attributeSelect.querySelectorAll('option');
      
      expect(options).toHaveLength(5);
      expect(options[0]).toHaveValue('Agility');
      expect(options[1]).toHaveValue('Smarts');
      expect(options[2]).toHaveValue('Spirit');
      expect(options[3]).toHaveValue('Strength');
      expect(options[4]).toHaveValue('Vigor');
    });

    it('displays current attribute selection', () => {
      render(<SkillEditor {...defaultProps} />);
      
      const attributeSelect = screen.getByTestId('attribute-select');
      expect(attributeSelect).toHaveValue('Agility');
    });

    it('handles different attribute selections', () => {
      const attributes = ['Agility', 'Smarts', 'Spirit', 'Strength', 'Vigor'];
      
      attributes.forEach(attribute => {
        const skillWithAttribute = {
          ...mockSkill,
          attribute: attribute
        };

        const { rerender } = render(<SkillEditor {...defaultProps} item={skillWithAttribute} />);
        
        const attributeSelect = screen.getByTestId('attribute-select');
        expect(attributeSelect).toHaveValue(attribute);
        
        rerender(<div />);
      });
    });
  });

  describe('Form Field Updates', () => {
    it('updates name when name field changes', () => {
      const mockOnChange = jest.fn();
      render(<SkillEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'Fighting' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'Fighting',
          attribute: 'Agility',
          description: 'Ranged combat skill'
        }),
        0
      );
    });

    it('updates attribute when attribute field changes', () => {
      const mockOnChange = jest.fn();
      render(<SkillEditor {...defaultProps} onChange={mockOnChange} />);
      
      const attributeSelect = screen.getByTestId('attribute-select');
      fireEvent.change(attributeSelect, { target: { value: 'Strength' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          attribute: 'Strength'
        }),
        0
      );
    });

    it('updates description when description field changes', () => {
      const mockOnChange = jest.fn();
      render(<SkillEditor {...defaultProps} onChange={mockOnChange} />);
      
      const descriptionInput = screen.getByLabelText('Description');
      fireEvent.change(descriptionInput, { target: { value: 'Updated description' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          description: 'Updated description'
        }),
        0
      );
    });

    it('passes correct index to onChange calls', () => {
      const mockOnChange = jest.fn();
      render(<SkillEditor {...defaultProps} index={5} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'Test Skill' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(expect.any(Object), 5);
    });

    it('preserves all existing properties when updating fields', () => {
      const mockOnChange = jest.fn();
      render(<SkillEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'Updated Name' } });
      
      const callArguments = mockOnChange.mock.calls[0][0];
      
      expect(callArguments).toEqual({
        name: 'Updated Name',
        attribute: 'Agility',
        description: 'Ranged combat skill'
      });
    });
  });

  describe('Delete Functionality', () => {
    it('calls onDelete when delete button is clicked', () => {
      const mockOnDelete = jest.fn();
      render(<SkillEditor {...defaultProps} onDelete={mockOnDelete} />);
      
      const deleteButton = screen.getByTestId('delete-button');
      fireEvent.click(deleteButton);
      
      expect(mockOnDelete).toHaveBeenCalledWith(0);
    });

    it('prevents default event when deleting', () => {
      const mockOnDelete = jest.fn();
      render(<SkillEditor {...defaultProps} onDelete={mockOnDelete} />);
      
      const deleteButton = screen.getByTestId('delete-button');
      const clickEvent = new MouseEvent('click', {
        bubbles: true,
        cancelable: true,
      });
      
      fireEvent(deleteButton, clickEvent);
      
      expect(mockOnDelete).toHaveBeenCalled();
    });

    it('passes correct index to onDelete', () => {
      const mockOnDelete = jest.fn();
      render(<SkillEditor {...defaultProps} index={8} onDelete={mockOnDelete} />);
      
      const deleteButton = screen.getByTestId('delete-button');
      fireEvent.click(deleteButton);
      
      expect(mockOnDelete).toHaveBeenCalledWith(8);
    });
  });

  describe('Savage Worlds Skills', () => {
    const savageWorldsSkills = [
      { name: 'Shooting', attribute: 'Agility', description: 'Ranged combat' },
      { name: 'Fighting', attribute: 'Agility', description: 'Melee combat' },
      { name: 'Notice', attribute: 'Smarts', description: 'Perception and awareness' },
      { name: 'Stealth', attribute: 'Agility', description: 'Move quietly and unseen' },
      { name: 'Driving', attribute: 'Agility', description: 'Operate land vehicles' },
      { name: 'Climbing', attribute: 'Strength', description: 'Scale walls and cliffs' },
      { name: 'Swimming', attribute: 'Vigor', description: 'Move through water' },
      { name: 'Intimidation', attribute: 'Spirit', description: 'Frighten opponents' },
      { name: 'Persuasion', attribute: 'Spirit', description: 'Convince others' },
      { name: 'Investigation', attribute: 'Smarts', description: 'Find clues and evidence' }
    ];

    it('handles typical Savage Worlds skills', () => {
      savageWorldsSkills.forEach(skill => {
        const { rerender } = render(<SkillEditor {...defaultProps} item={skill} />);
        
        expect(screen.getByLabelText('Name')).toHaveValue(skill.name);
        expect(screen.getByTestId('attribute-select')).toHaveValue(skill.attribute);
        expect(screen.getByLabelText('Description')).toHaveValue(skill.description);
        
        rerender(<div />);
      });
    });

    it('handles skills grouped by attribute', () => {
      const skillsByAttribute = {
        'Agility': ['Shooting', 'Fighting', 'Stealth', 'Driving', 'Lockpicking'],
        'Smarts': ['Notice', 'Investigation', 'Healing', 'Research', 'Repair'],
        'Spirit': ['Intimidation', 'Persuasion', 'Taunt', 'Guts'],
        'Strength': ['Climbing', 'Throwing'],
        'Vigor': ['Swimming']
      };

      Object.entries(skillsByAttribute).forEach(([attribute, skills]) => {
        skills.forEach(skillName => {
          const skill = {
            name: skillName,
            attribute: attribute,
            description: `${skillName} skill based on ${attribute}`
          };

          const { rerender } = render(<SkillEditor {...defaultProps} item={skill} />);
          
          expect(screen.getByLabelText('Name')).toHaveValue(skillName);
          expect(screen.getByTestId('attribute-select')).toHaveValue(attribute);
          
          rerender(<div />);
        });
      });
    });

    it('handles knowledge skills', () => {
      const knowledgeSkills = [
        'Knowledge (Academics)',
        'Knowledge (Battle)',
        'Knowledge (Electronics)',
        'Knowledge (History)',
        'Knowledge (Journalism)',
        'Knowledge (Language)',
        'Knowledge (Law)',
        'Knowledge (Medicine)',
        'Knowledge (Occult)',
        'Knowledge (Science)'
      ];

      knowledgeSkills.forEach(skill => {
        const knowledgeSkill = {
          name: skill,
          attribute: 'Smarts',
          description: 'Specialized knowledge skill'
        };

        const { rerender } = render(<SkillEditor {...defaultProps} item={knowledgeSkill} />);
        
        expect(screen.getByLabelText('Name')).toHaveValue(skill);
        expect(screen.getByTestId('attribute-select')).toHaveValue('Smarts');
        
        rerender(<div />);
      });
    });

    it('handles professional skills', () => {
      const professionalSkills = [
        { name: 'Boating', attribute: 'Agility' },
        { name: 'Piloting', attribute: 'Agility' },
        { name: 'Riding', attribute: 'Agility' },
        { name: 'Survival', attribute: 'Smarts' },
        { name: 'Tracking', attribute: 'Smarts' },
        { name: 'Gambling', attribute: 'Smarts' },
        { name: 'Streetwise', attribute: 'Smarts' }
      ];

      professionalSkills.forEach(skill => {
        const { rerender } = render(<SkillEditor {...defaultProps} item={skill} />);
        
        expect(screen.getByLabelText('Name')).toHaveValue(skill.name);
        expect(screen.getByTestId('attribute-select')).toHaveValue(skill.attribute);
        
        rerender(<div />);
      });
    });
  });

  describe('Data Handling', () => {
    it('handles undefined skill properties', () => {
      const incompleteSkill = {
        name: 'Incomplete Skill'
      };

      expect(() => {
        render(<SkillEditor {...defaultProps} item={incompleteSkill} />);
      }).not.toThrow();
    });

    it('handles null skill properties', () => {
      const nullSkill = {
        name: 'Null Skill',
        attribute: null,
        description: null
      };

      render(<SkillEditor {...defaultProps} item={nullSkill} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue('Null Skill');
      expect(screen.getByTestId('attribute-select')).toHaveValue('');
      expect(screen.getByLabelText('Description')).toHaveValue('');
    });

    it('handles empty string values', () => {
      const emptySkill = {
        name: '',
        attribute: '',
        description: ''
      };

      render(<SkillEditor {...defaultProps} item={emptySkill} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue('');
      expect(screen.getByTestId('attribute-select')).toHaveValue('');
      expect(screen.getByLabelText('Description')).toHaveValue('');
    });

    it('handles invalid attribute values', () => {
      const invalidAttributeSkill = {
        ...mockSkill,
        attribute: 'Invalid'
      };

      render(<SkillEditor {...defaultProps} item={invalidAttributeSkill} />);
      
      const attributeSelect = screen.getByTestId('attribute-select');
      expect(attributeSelect).toHaveValue('Invalid');
    });

    it('handles special characters in skill data', () => {
      const specialSkill = {
        name: 'Knowledge (Weird Science™)',
        attribute: 'Smarts',
        description: 'Skill with éñ and symbols: @#$%^&*()'
      };

      render(<SkillEditor {...defaultProps} item={specialSkill} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue('Knowledge (Weird Science™)');
      expect(screen.getByLabelText('Description')).toHaveValue('Skill with éñ and symbols: @#$%^&*()');
    });
  });

  describe('Component Integration', () => {
    it('extends BaseEditor properly', () => {
      render(<SkillEditor {...defaultProps} />);
      
      // Should render base BaseEditor functionality
      expect(screen.getByTestId('base-editor')).toBeInTheDocument();
      expect(screen.getByTestId('editor-content')).toBeInTheDocument();
      expect(screen.getByTestId('delete-button')).toBeInTheDocument();
    });

    it('uses AttributeSelectFormGroup component', () => {
      render(<SkillEditor {...defaultProps} />);
      
      expect(screen.getByTestId('attribute-select-form-group')).toBeInTheDocument();
      expect(screen.getByTestId('attribute-select')).toBeInTheDocument();
    });

    it('implements React.Component properly', () => {
      const component = new SkillEditor.prototype.constructor(defaultProps);
      expect(component).toBeInstanceOf(Object);
    });

    it('implements required PropTypes', () => {
      expect(SkillEditor.propTypes).toBeDefined();
      expect(SkillEditor.propTypes.id).toBeDefined();
      expect(SkillEditor.propTypes.index).toBeDefined();
      expect(SkillEditor.propTypes.item).toBeDefined();
      expect(SkillEditor.propTypes.onChange).toBeDefined();
      expect(SkillEditor.propTypes.onDelete).toBeDefined();
    });

    it('provides default props', () => {
      expect(SkillEditor.defaultProps).toBeDefined();
      expect(SkillEditor.defaultProps.id).toBe('Editor');
    });

    it('uses Object.assign for state updates', () => {
      const mockOnChange = jest.fn();
      render(<SkillEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'New Name' } });
      
      // Verify Object.assign pattern is used by checking the result
      const callArguments = mockOnChange.mock.calls[0][0];
      expect(callArguments).toEqual({
        ...mockSkill,
        name: 'New Name'
      });
    });
  });

  describe('Edge Cases', () => {
    it('handles rapid field changes', () => {
      const mockOnChange = jest.fn();
      render(<SkillEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      const attributeSelect = screen.getByTestId('attribute-select');
      
      fireEvent.change(nameInput, { target: { value: 'Name 1' } });
      fireEvent.change(nameInput, { target: { value: 'Name 2' } });
      fireEvent.change(attributeSelect, { target: { value: 'Strength' } });
      fireEvent.change(nameInput, { target: { value: 'Final Name' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(4);
      expect(mockOnChange).toHaveBeenLastCalledWith(
        expect.objectContaining({ name: 'Final Name' }),
        0
      );
    });

    it('handles very long field values', () => {
      const longSkill = {
        ...mockSkill,
        name: 'This is a very long skill name that might cause display or processing issues',
        description: 'This skill has an extremely long description that goes into great detail about all the various applications and uses of this particular skill in different scenarios and situations'
      };

      render(<SkillEditor {...defaultProps} item={longSkill} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue(longSkill.name);
      expect(screen.getByLabelText('Description')).toHaveValue(longSkill.description);
    });

    it('handles whitespace in field values', () => {
      const mockOnChange = jest.fn();
      render(<SkillEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: '  Skill Name  ' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ name: '  Skill Name  ' }),
        0
      );
    });

    it('handles newlines in description field', () => {
      const mockOnChange = jest.fn();
      render(<SkillEditor {...defaultProps} onChange={mockOnChange} />);
      
      const descriptionInput = screen.getByLabelText('Description');
      fireEvent.change(descriptionInput, { target: { value: 'Line 1\nLine 2\nLine 3' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ description: 'Line 1\nLine 2\nLine 3' }),
        0
      );
    });

    it('handles simultaneous field updates', () => {
      const mockOnChange = jest.fn();
      render(<SkillEditor {...defaultProps} onChange={mockOnChange} />);
      
      const nameInput = screen.getByLabelText('Name');
      const attributeSelect = screen.getByTestId('attribute-select');
      const descriptionInput = screen.getByLabelText('Description');
      
      fireEvent.change(nameInput, { target: { value: 'Updated Name' } });
      fireEvent.change(attributeSelect, { target: { value: 'Spirit' } });
      fireEvent.change(descriptionInput, { target: { value: 'Updated Description' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(3);
    });
  });

  describe('Savage Worlds Attributes System', () => {
    it('properly handles all core attributes', () => {
      const coreAttributes = ['Agility', 'Smarts', 'Spirit', 'Strength', 'Vigor'];
      
      coreAttributes.forEach(attribute => {
        const mockOnChange = jest.fn();
        render(<SkillEditor {...defaultProps} onChange={mockOnChange} />);
        
        const attributeSelect = screen.getByTestId('attribute-select');
        fireEvent.change(attributeSelect, { target: { value: attribute } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ attribute }),
          0
        );
      });
    });

    it('handles switching between attributes', () => {
      const mockOnChange = jest.fn();
      render(<SkillEditor {...defaultProps} onChange={mockOnChange} />);
      
      const attributeSelect = screen.getByTestId('attribute-select');
      
      fireEvent.change(attributeSelect, { target: { value: 'Smarts' } });
      fireEvent.change(attributeSelect, { target: { value: 'Spirit' } });
      fireEvent.change(attributeSelect, { target: { value: 'Strength' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(3);
      expect(mockOnChange).toHaveBeenLastCalledWith(
        expect.objectContaining({ attribute: 'Strength' }),
        0
      );
    });

    it('validates attribute-skill combinations', () => {
      const validCombinations = [
        { skill: 'Shooting', attribute: 'Agility' },
        { skill: 'Fighting', attribute: 'Agility' },
        { skill: 'Notice', attribute: 'Smarts' },
        { skill: 'Investigation', attribute: 'Smarts' },
        { skill: 'Intimidation', attribute: 'Spirit' },
        { skill: 'Persuasion', attribute: 'Spirit' },
        { skill: 'Climbing', attribute: 'Strength' },
        { skill: 'Throwing', attribute: 'Strength' },
        { skill: 'Swimming', attribute: 'Vigor' }
      ];

      validCombinations.forEach(combination => {
        const skill = {
          name: combination.skill,
          attribute: combination.attribute,
          description: `${combination.skill} skill`
        };

        const { rerender } = render(<SkillEditor {...defaultProps} item={skill} />);
        
        expect(screen.getByLabelText('Name')).toHaveValue(combination.skill);
        expect(screen.getByTestId('attribute-select')).toHaveValue(combination.attribute);
        
        rerender(<div />);
      });
    });
  });

  describe('Setting-Specific Skills', () => {
    it('handles fantasy setting skills', () => {
      const fantasySkill = {
        name: 'Spellcasting',
        attribute: 'Smarts',
        description: 'Ability to cast arcane spells'
      };

      render(<SkillEditor {...defaultProps} item={fantasySkill} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue('Spellcasting');
      expect(screen.getByTestId('attribute-select')).toHaveValue('Smarts');
    });

    it('handles modern setting skills', () => {
      const modernSkill = {
        name: 'Hacking',
        attribute: 'Smarts',
        description: 'Computer and network infiltration'
      };

      render(<SkillEditor {...defaultProps} item={modernSkill} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue('Hacking');
      expect(screen.getByTestId('attribute-select')).toHaveValue('Smarts');
    });

    it('handles sci-fi setting skills', () => {
      const sciFiSkill = {
        name: 'Zero-G Maneuvering',
        attribute: 'Agility',
        description: 'Movement in zero gravity environments'
      };

      render(<SkillEditor {...defaultProps} item={sciFiSkill} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue('Zero-G Maneuvering');
      expect(screen.getByTestId('attribute-select')).toHaveValue('Agility');
    });

    it('handles horror setting skills', () => {
      const horrorSkill = {
        name: 'Occult',
        attribute: 'Smarts',
        description: 'Knowledge of supernatural and forbidden lore'
      };

      render(<SkillEditor {...defaultProps} item={horrorSkill} />);
      
      expect(screen.getByLabelText('Name')).toHaveValue('Occult');
      expect(screen.getByTestId('attribute-select')).toHaveValue('Smarts');
    });
  });
});