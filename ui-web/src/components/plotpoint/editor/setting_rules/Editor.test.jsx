import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import Editor from './Editor';

// Mock the dependencies
jest.mock('bootstrap-react-components', () => ({
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
  )
}));

jest.mock('../../../BaseEditor', () => {
  return function MockBaseEditor({ id, onDelete, children }) {
    return (
      <div data-testid={`base-editor-${id}`}>
        <button onClick={onDelete} data-testid={`delete-button-${id}`}>
          Delete
        </button>
        {children}
      </div>
    );
  };
});

describe('Setting Rules Editor Component', () => {
  const defaultProps = {
    id: 'test-setting-rule',
    name: 'Born a Hero',
    description: 'Characters start with an additional Edge at character creation',
    nameChange: jest.fn(),
    descriptionChange: jest.fn(),
    onDelete: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders without crashing', () => {
      render(<Editor {...defaultProps} />);
      expect(screen.getByTestId('base-editor-SettingRuleEditor-test-setting-rule')).toBeInTheDocument();
    });

    it('renders all form fields with correct labels', () => {
      render(<Editor {...defaultProps} />);
      
      expect(screen.getByText('Name')).toBeInTheDocument();
      expect(screen.getByText('Description')).toBeInTheDocument();
    });

    it('displays field values correctly', () => {
      render(<Editor {...defaultProps} />);
      
      expect(screen.getByDisplayValue('Born a Hero')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Characters start with an additional Edge at character creation')).toBeInTheDocument();
    });

    it('generates correct component IDs', () => {
      render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-SettingRuleEditor-test-setting-rule-Name');
      const descTextarea = screen.getByTestId('textarea-SettingRuleEditor-test-setting-rule-Description');
      
      expect(nameInput).toBeInTheDocument();
      expect(descTextarea).toBeInTheDocument();
    });
  });

  describe('Form Field Updates', () => {
    it('calls nameChange when name field is updated', () => {
      render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-SettingRuleEditor-test-setting-rule-Name');
      fireEvent.change(nameInput, { target: { value: 'Gritty Damage' } });
      
      expect(defaultProps.nameChange).toHaveBeenCalledTimes(1);
    });

    it('calls descriptionChange when description field is updated', () => {
      render(<Editor {...defaultProps} />);
      
      const descTextarea = screen.getByTestId('textarea-SettingRuleEditor-test-setting-rule-Description');
      fireEvent.change(descTextarea, { target: { value: 'Damage is more realistic and dangerous' } });
      
      expect(defaultProps.descriptionChange).toHaveBeenCalledTimes(1);
    });

    it('handles multiple field updates correctly', () => {
      render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-SettingRuleEditor-test-setting-rule-Name');
      const descTextarea = screen.getByTestId('textarea-SettingRuleEditor-test-setting-rule-Description');
      
      fireEvent.change(nameInput, { target: { value: 'Multiple Foes' } });
      fireEvent.change(descTextarea, { target: { value: 'Characters face multiple opponents more often' } });
      
      expect(defaultProps.nameChange).toHaveBeenCalledTimes(1);
      expect(defaultProps.descriptionChange).toHaveBeenCalledTimes(1);
    });
  });

  describe('Delete Functionality', () => {
    it('calls onDelete with correct parameters when delete button is clicked', () => {
      render(<Editor {...defaultProps} />);
      
      const deleteButton = screen.getByTestId('delete-button-SettingRuleEditor-test-setting-rule');
      fireEvent.click(deleteButton);
      
      expect(defaultProps.onDelete).toHaveBeenCalledWith({
        name: 'Born a Hero',
        description: 'Characters start with an additional Edge at character creation'
      });
    });

    it('prevents default event when delete is triggered', () => {
      const mockEvent = { preventDefault: jest.fn() };
      const component = new Editor(defaultProps);
      
      component.onDelete(mockEvent);
      
      expect(mockEvent.preventDefault).toHaveBeenCalled();
      expect(defaultProps.onDelete).toHaveBeenCalledWith({
        name: 'Born a Hero',
        description: 'Characters start with an additional Edge at character creation'
      });
    });

    it('handles delete with empty name and description', () => {
      const emptyProps = { ...defaultProps, name: '', description: '' };
      const component = new Editor(emptyProps);
      const mockEvent = { preventDefault: jest.fn() };
      
      component.onDelete(mockEvent);
      
      expect(defaultProps.onDelete).toHaveBeenCalledWith({
        name: '',
        description: ''
      });
    });
  });

  describe('Savage Worlds Setting Rules', () => {
    const standardSettingRules = [
      {
        name: 'Born a Hero',
        description: 'Characters start with an additional Edge at character creation'
      },
      {
        name: 'Gritty Damage',
        description: 'Damage is more realistic and dangerous. Characters have three wounds instead of three'
      },
      {
        name: 'Multiple Languages',
        description: 'Characters with d6+ Smarts know additional languages equal to half their Smarts die'
      },
      {
        name: 'More Skill Points',
        description: 'Characters get additional skill points during character creation'
      },
      {
        name: 'Conviction',
        description: 'Players earn Conviction tokens for exceptional roleplay, which provide bonuses'
      },
      {
        name: 'Joker\'s Wild',
        description: 'Drawing a Joker in combat allows for spectacular results'
      },
      {
        name: 'Blood & Guts',
        description: 'Damage rolls get +2 when dealing with blood and gore'
      },
      {
        name: 'High Adventure',
        description: 'Characters start with additional Power Points for supernatural campaigns'
      }
    ];

    it('handles standard Savage Worlds setting rules', () => {
      standardSettingRules.forEach((rule, index) => {
        const props = { 
          ...defaultProps, 
          name: rule.name, 
          description: rule.description,
          id: `rule-${index}` 
        };
        
        const { container } = render(<Editor {...props} />);
        
        expect(container.querySelector('input[type="text"]')).toHaveValue(rule.name);
        expect(container.querySelector('textarea')).toHaveValue(rule.description);
      });
    });

    it('handles custom setting rules', () => {
      const customRules = [
        {
          name: 'Cyberpunk Implants',
          description: 'Characters can have cybernetic implants that provide mechanical benefits'
        },
        {
          name: 'Magic is Rare',
          description: 'Arcane backgrounds cost double the normal points during creation'
        },
        {
          name: 'Heroic Sacrifice',
          description: 'Characters can sacrifice themselves for dramatic effect and story impact'
        }
      ];
      
      customRules.forEach(rule => {
        const props = { ...defaultProps, name: rule.name, description: rule.description };
        const { container } = render(<Editor {...props} />);
        
        expect(container.querySelector('input[type="text"]')).toHaveValue(rule.name);
        expect(container.querySelector('textarea')).toHaveValue(rule.description);
      });
    });

    it('handles genre-specific setting rules', () => {
      const genreRules = {
        Horror: [
          {
            name: 'Fear Table',
            description: 'Use expanded fear effects for horror encounters'
          },
          {
            name: 'Sanity',
            description: 'Characters have a Sanity attribute that can be damaged by horrific events'
          }
        ],
        SciFi: [
          {
            name: 'Zero-G Combat',
            description: 'Special rules for fighting in zero gravity environments'
          },
          {
            name: 'Tech Level',
            description: 'Equipment availability varies based on technological advancement'
          }
        ],
        Fantasy: [
          {
            name: 'Spell Components',
            description: 'Spellcasters must have material components to cast certain spells'
          },
          {
            name: 'Divine Favor',
            description: 'Characters can gain favor with deities for supernatural aid'
          }
        ]
      };
      
      Object.entries(genreRules).forEach(([genre, rules]) => {
        rules.forEach((rule, index) => {
          const props = { 
            ...defaultProps, 
            name: rule.name, 
            description: rule.description,
            id: `${genre.toLowerCase()}-${index}`
          };
          
          const { container } = render(<Editor {...props} />);
          
          expect(container.querySelector('input[type="text"]')).toHaveValue(rule.name);
          expect(container.querySelector('textarea')).toHaveValue(rule.description);
        });
      });
    });
  });

  describe('Accessibility', () => {
    it('has name field marked as required', () => {
      render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-SettingRuleEditor-test-setting-rule-Name');
      expect(nameInput).toHaveAttribute('required');
    });

    it('has proper field types', () => {
      render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-SettingRuleEditor-test-setting-rule-Name');
      expect(nameInput).toHaveAttribute('type', 'text');
    });

    it('has proper labels for form fields', () => {
      render(<Editor {...defaultProps} />);
      
      expect(screen.getByText('Name')).toBeInTheDocument();
      expect(screen.getByText('Description')).toBeInTheDocument();
    });
  });

  describe('Edge Cases', () => {
    it('handles undefined name gracefully', () => {
      const propsWithUndefined = {
        ...defaultProps,
        name: undefined
      };
      
      expect(() => render(<Editor {...propsWithUndefined} />)).not.toThrow();
    });

    it('handles null description gracefully', () => {
      const propsWithNull = {
        ...defaultProps,
        description: null
      };
      
      expect(() => render(<Editor {...propsWithNull} />)).not.toThrow();
    });

    it('handles empty string values', () => {
      const propsWithEmpty = {
        ...defaultProps,
        name: '',
        description: ''
      };
      
      render(<Editor {...propsWithEmpty} />);
      
      const nameInput = screen.getByTestId('text-input-SettingRuleEditor-test-setting-rule-Name');
      const descTextarea = screen.getByTestId('textarea-SettingRuleEditor-test-setting-rule-Description');
      
      expect(nameInput.value).toBe('');
      expect(descTextarea.value).toBe('');
    });

    it('handles very long text inputs', () => {
      const longName = 'A'.repeat(500);
      const longDescription = 'B'.repeat(2000);
      const propsWithLongText = {
        ...defaultProps,
        name: longName,
        description: longDescription
      };
      
      render(<Editor {...propsWithLongText} />);
      
      expect(screen.getByDisplayValue(longName)).toBeInTheDocument();
      expect(screen.getByDisplayValue(longDescription)).toBeInTheDocument();
    });

    it('handles special characters in text inputs', () => {
      const specialName = 'Rule with quotes and symbols';
      const specialDescription = 'Description with newlines and symbols';
      const propsWithSpecialChars = {
        ...defaultProps,
        name: specialName,
        description: specialDescription
      };
      
      render(<Editor {...propsWithSpecialChars} />);
      
      expect(screen.getByDisplayValue(specialName)).toBeInTheDocument();
      expect(screen.getByDisplayValue(specialDescription)).toBeInTheDocument();
    });

    it('handles Unicode characters correctly', () => {
      const unicodeName = 'Règle Spéciale';
      const unicodeDescription = 'Descripción con caracteres especiales: ñ, é, ü, 中文';
      const propsWithUnicode = {
        ...defaultProps,
        name: unicodeName,
        description: unicodeDescription
      };
      
      render(<Editor {...propsWithUnicode} />);
      
      expect(screen.getByDisplayValue(unicodeName)).toBeInTheDocument();
      expect(screen.getByDisplayValue(unicodeDescription)).toBeInTheDocument();
    });
  });

  describe('Component Integration', () => {
    it('passes correct props to BaseEditor', () => {
      render(<Editor {...defaultProps} />);
      
      const baseEditor = screen.getByTestId('base-editor-SettingRuleEditor-test-setting-rule');
      expect(baseEditor).toBeInTheDocument();
      
      const deleteButton = screen.getByTestId('delete-button-SettingRuleEditor-test-setting-rule');
      expect(deleteButton).toBeInTheDocument();
    });

    it('maintains component state consistency across updates', () => {
      const { rerender } = render(<Editor {...defaultProps} />);
      
      const updatedProps = {
        ...defaultProps,
        name: 'Updated Rule',
        description: 'Updated description'
      };
      
      rerender(<Editor {...updatedProps} />);
      
      expect(screen.getByDisplayValue('Updated Rule')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Updated description')).toBeInTheDocument();
    });

    it('handles multiple instances with different IDs', () => {
      const props1 = { ...defaultProps, id: 'rule-1' };
      const props2 = { ...defaultProps, id: 'rule-2' };
      
      const { container } = render(
        <div>
          <Editor {...props1} />
          <Editor {...props2} />
        </div>
      );
      
      expect(container.querySelectorAll('[data-testid^="base-editor-"]')).toHaveLength(2);
    });

    it('calls appropriate handlers on field changes', () => {
      render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-SettingRuleEditor-test-setting-rule-Name');
      const descTextarea = screen.getByTestId('textarea-SettingRuleEditor-test-setting-rule-Description');
      
      fireEvent.change(nameInput, { target: { value: 'Test Name' } });
      fireEvent.change(descTextarea, { target: { value: 'Test Description' } });
      
      expect(defaultProps.nameChange).toHaveBeenCalledTimes(1);
      expect(defaultProps.descriptionChange).toHaveBeenCalledTimes(1);
    });
  });

  describe('Form Validation Scenarios', () => {
    it('handles required field validation', () => {
      render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-SettingRuleEditor-test-setting-rule-Name');
      expect(nameInput).toHaveAttribute('required');
    });

    it('handles valid form data', () => {
      const validProps = {
        ...defaultProps,
        name: 'Valid Setting Rule',
        description: 'A valid setting rule description'
      };
      
      render(<Editor {...validProps} />);
      
      expect(screen.getByDisplayValue('Valid Setting Rule')).toBeInTheDocument();
      expect(screen.getByDisplayValue('A valid setting rule description')).toBeInTheDocument();
    });

    it('handles form submission with all required fields', () => {
      const completeProps = {
        ...defaultProps,
        name: 'Complete Rule',
        description: 'This rule has all required fields filled out properly'
      };
      
      render(<Editor {...completeProps} />);
      
      const nameInput = screen.getByTestId('text-input-SettingRuleEditor-test-setting-rule-Name');
      const descTextarea = screen.getByTestId('textarea-SettingRuleEditor-test-setting-rule-Description');
      
      expect(nameInput).toHaveAttribute('required');
      expect(nameInput.value).toBe('Complete Rule');
      expect(descTextarea.value).toBe('This rule has all required fields filled out properly');
    });
  });

  describe('Component Lifecycle', () => {
    it('renders consistently on mount and unmount', () => {
      const { unmount } = render(<Editor {...defaultProps} />);
      
      expect(screen.getByTestId('base-editor-SettingRuleEditor-test-setting-rule')).toBeInTheDocument();
      
      unmount();
    });

    it('handles prop changes correctly', () => {
      const { rerender } = render(<Editor {...defaultProps} />);
      
      const newProps = {
        ...defaultProps,
        name: 'Changed Name',
        description: 'Changed Description',
        id: 'changed-id'
      };
      
      rerender(<Editor {...newProps} />);
      
      expect(screen.getByDisplayValue('Changed Name')).toBeInTheDocument();
      expect(screen.getByDisplayValue('Changed Description')).toBeInTheDocument();
    });

    it('maintains proper event handler binding', () => {
      render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-SettingRuleEditor-test-setting-rule-Name');
      
      // Test multiple events to ensure handlers are properly bound
      fireEvent.change(nameInput, { target: { value: 'First Change' } });
      fireEvent.change(nameInput, { target: { value: 'Second Change' } });
      
      expect(defaultProps.nameChange).toHaveBeenCalledTimes(2);
    });
  });

  describe('Error Handling', () => {
    it('handles missing callback functions gracefully', () => {
      const propsWithoutCallbacks = {
        ...defaultProps,
        nameChange: undefined,
        descriptionChange: undefined,
        onDelete: undefined
      };
      
      expect(() => render(<Editor {...propsWithoutCallbacks} />)).not.toThrow();
    });

    it('handles malformed prop values', () => {
      const malformedProps = {
        ...defaultProps,
        name: {},
        description: [],
        id: null
      };
      
      expect(() => render(<Editor {...malformedProps} />)).not.toThrow();
    });
  });

  describe('Performance Considerations', () => {
    it('renders efficiently with minimal re-renders', () => {
      const { rerender } = render(<Editor {...defaultProps} />);
      
      // Rerender with same props should not cause issues
      rerender(<Editor {...defaultProps} />);
      rerender(<Editor {...defaultProps} />);
      
      expect(screen.getByDisplayValue('Born a Hero')).toBeInTheDocument();
    });

    it('handles rapid input changes', () => {
      render(<Editor {...defaultProps} />);
      
      const nameInput = screen.getByTestId('text-input-SettingRuleEditor-test-setting-rule-Name');
      
      // Simulate rapid typing
      for (let i = 0; i < 10; i++) {
        fireEvent.change(nameInput, { target: { value: `Test${i}` } });
      }
      
      expect(defaultProps.nameChange).toHaveBeenCalledTimes(10);
    });
  });

  describe('Complex Setting Rules', () => {
    it('handles multi-line descriptions with formatting', () => {
      const complexDescription = 'This is a complex setting rule that includes multiple elements and line breaks for special mechanics and dice rolls.';
      
      const props = { ...defaultProps, description: complexDescription };
      render(<Editor {...props} />);
      
      expect(screen.getByDisplayValue(complexDescription)).toBeInTheDocument();
    });

    it('handles rules with numeric and mechanical elements', () => {
      const mechanicalRules = [
        {
          name: 'Extra Edges',
          description: 'Characters gain +1 Edge every 3 advances instead of every 4'
        },
        {
          name: 'Attribute Increase',
          description: 'Raising attributes costs 1 less advance point (minimum 1)'
        },
        {
          name: 'Skill Bonus',
          description: 'All skill rolls get +1 bonus in specific circumstances'
        }
      ];
      
      mechanicalRules.forEach(rule => {
        const props = { ...defaultProps, name: rule.name, description: rule.description };
        const { container } = render(<Editor {...props} />);
        
        expect(container.querySelector('input[type="text"]')).toHaveValue(rule.name);
        expect(container.querySelector('textarea')).toHaveValue(rule.description);
      });
    });
  });
});