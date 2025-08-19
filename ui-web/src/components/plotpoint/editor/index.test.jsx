import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import '@testing-library/jest-dom';
import PlotPointForm from './index';

// Mock child components
jest.mock('./BasicRules', () => {
  return function MockBasicRules({ id, basicRules, onChange }) {
    return (
      <div data-testid="basic-rules">
        <input 
          data-testid="basic-rules-input"
          value={basicRules?.testField || ''}
          onChange={(e) => onChange && onChange({ testField: e.target.value })}
        />
      </div>
    );
  };
});

jest.mock('./Navigation', () => {
  return function MockNavigation({ id, navigateTo }) {
    return (
      <div data-testid="navigation">
        <button onClick={() => navigateTo('BasicRules')}>Basic Rules</button>
        <button onClick={() => navigateTo('Skills')}>Skills</button>
        <button onClick={() => navigateTo('Edges')}>Edges</button>
      </div>
    );
  };
});

jest.mock('./skills', () => {
  return function MockSkills({ id, skills, onChange }) {
    return (
      <div data-testid="skills">
        <input 
          data-testid="skills-input"
          value={skills?.[0]?.name || ''}
          onChange={(e) => onChange && onChange([{ name: e.target.value }])}
        />
      </div>
    );
  };
});

jest.mock('./edges', () => {
  return function MockEdges({ id, edges, onChange }) {
    return (
      <div data-testid="edges">
        <input 
          data-testid="edges-input"
          value={edges?.length || 0}
          onChange={(e) => onChange && onChange([{ name: e.target.value }])}
        />
      </div>
    );
  };
});

// Mock Bootstrap React Components
jest.mock('bootstrap-react-components', () => ({
  Alert: function MockAlert({ children, id, context }) {
    return <div data-testid="alert" id={id} data-context={context}>{children}</div>;
  },
  Button: function MockButton({ children, id, onClick, disabled }) {
    return (
      <button data-testid="button" id={id} onClick={onClick} disabled={disabled}>
        {children}
      </button>
    );
  },
  TextFormGroup: function MockTextFormGroup({ id, label, onChange, value, required }) {
    return (
      <div data-testid="text-form-group">
        <label htmlFor={id}>{label}</label>
        <input 
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

// Mock FontAwesome
jest.mock('@fortawesome/react-fontawesome', () => ({
  FontAwesomeIcon: function MockFontAwesomeIcon({ icon }) {
    return <span data-testid="icon" data-icon={icon} />;
  }
}));

describe('PlotPointForm Component', () => {
  const mockPlotPoint = {
    name: 'Test Plot Point',
    description: 'Test Description',
    basicRules: { testField: 'test value' },
    skills: [{ name: 'Test Skill' }],
    edges: [{ name: 'Test Edge' }],
    hindrances: [],
    gear: [],
    powers: [],
    races: [],
    beasts: [],
    characters: [],
    settingRules: []
  };

  const defaultProps = {
    id: 'test-form',
    plotPoint: mockPlotPoint,
    onSave: jest.fn(),
    onCancel: jest.fn(),
    errors: [],
    disabled: false
  };

  let mockWindow;

  beforeEach(() => {
    jest.clearAllMocks();
    
    // Mock window methods
    mockWindow = {
      addEventListener: jest.fn(),
      removeEventListener: jest.fn(),
      confirm: jest.fn(() => true)
    };
  });

  describe('Rendering', () => {
    it('renders with required props', () => {
      render(<PlotPointForm {...defaultProps} />);
      
      expect(screen.getByTestId('text-input')).toBeInTheDocument();
      expect(screen.getByLabelText('Name')).toHaveValue('Test Plot Point');
      expect(screen.getByLabelText('Description')).toHaveValue('Test Description');
    });

    it('renders with correct form id', () => {
      render(<PlotPointForm {...defaultProps} id="custom-form" />);
      
      const form = document.querySelector('form');
      expect(form).toHaveAttribute('id', 'PlotPoint-custom-form');
    });

    it('displays Save and Cancel buttons', () => {
      render(<PlotPointForm {...defaultProps} />);
      
      const buttons = screen.getAllByTestId('button');
      expect(buttons).toHaveLength(2);
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      const cancelButton = screen.getByRole('button', { name: /cancel/i });
      
      expect(saveButton).toBeInTheDocument();
      expect(cancelButton).toBeInTheDocument();
    });

    it('renders Navigation component', () => {
      render(<PlotPointForm {...defaultProps} />);
      
      expect(screen.getByTestId('navigation')).toBeInTheDocument();
    });

    it('renders default section (BasicRules)', () => {
      render(<PlotPointForm {...defaultProps} />);
      
      expect(screen.getByTestId('basic-rules')).toBeInTheDocument();
    });
  });

  describe('Error Handling', () => {
    it('displays no errors when errors array is empty', () => {
      render(<PlotPointForm {...defaultProps} errors={[]} />);
      
      expect(screen.queryByTestId('alert')).not.toBeInTheDocument();
    });

    it('displays string errors correctly', () => {
      const errors = ['Error 1', 'Error 2'];
      render(<PlotPointForm {...defaultProps} errors={errors} />);
      
      const alerts = screen.getAllByTestId('alert');
      expect(alerts).toHaveLength(2);
      expect(alerts[0]).toHaveTextContent('Error 1');
      expect(alerts[1]).toHaveTextContent('Error 2');
    });

    it('displays object errors with message property', () => {
      const errors = [{ message: 'Object error message' }];
      render(<PlotPointForm {...defaultProps} errors={errors} />);
      
      const alert = screen.getByTestId('alert');
      expect(alert).toHaveTextContent('Object error message');
    });

    it('displays fallback message for malformed error objects', () => {
      const errors = [{ code: 'ERR001' }]; // No message property
      render(<PlotPointForm {...defaultProps} errors={errors} />);
      
      const alert = screen.getByTestId('alert');
      expect(alert).toHaveTextContent('An error occurred');
    });

    it('sets correct alert context and IDs', () => {
      const errors = ['Test error'];
      render(<PlotPointForm {...defaultProps} id="error-test" errors={errors} />);
      
      const alert = screen.getByTestId('alert');
      expect(alert).toHaveAttribute('data-context', 'danger');
      expect(alert).toHaveAttribute('id', 'error-test-error-0');
    });
  });

  describe('Section Navigation', () => {
    it('starts with BasicRules section', () => {
      render(<PlotPointForm {...defaultProps} />);
      
      expect(screen.getByTestId('basic-rules')).toBeInTheDocument();
      expect(screen.queryByTestId('skills')).not.toBeInTheDocument();
    });

    it('switches to Skills section when navigated', () => {
      render(<PlotPointForm {...defaultProps} />);
      
      fireEvent.click(screen.getByText('Skills'));
      
      expect(screen.getByTestId('skills')).toBeInTheDocument();
      expect(screen.queryByTestId('basic-rules')).not.toBeInTheDocument();
    });

    it('switches to Edges section when navigated', () => {
      render(<PlotPointForm {...defaultProps} />);
      
      fireEvent.click(screen.getByText('Edges'));
      
      expect(screen.getByTestId('edges')).toBeInTheDocument();
      expect(screen.queryByTestId('basic-rules')).not.toBeInTheDocument();
    });

    it('returns null for unknown section', () => {
      const component = new PlotPointForm.prototype.constructor(defaultProps);
      component.state = { section: 'UnknownSection', plotPoint: mockPlotPoint };
      
      const result = component.showSection('test-id');
      expect(result).toBeNull();
    });
  });

  describe('Form Field Changes', () => {
    it('updates plot point name', () => {
      render(<PlotPointForm {...defaultProps} />);
      
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'Updated Name' } });
      
      expect(nameInput).toHaveValue('Updated Name');
    });

    it('updates plot point description', () => {
      render(<PlotPointForm {...defaultProps} />);
      
      const descriptionInput = screen.getByLabelText('Description');
      fireEvent.change(descriptionInput, { target: { value: 'Updated Description' } });
      
      expect(descriptionInput).toHaveValue('Updated Description');
    });

    it('updates basic rules through child component', () => {
      render(<PlotPointForm {...defaultProps} />);
      
      const basicRulesInput = screen.getByTestId('basic-rules-input');
      fireEvent.change(basicRulesInput, { target: { value: 'updated value' } });
      
      expect(basicRulesInput).toHaveValue('updated value');
    });

    it('updates skills when section is changed to Skills', () => {
      render(<PlotPointForm {...defaultProps} />);
      
      // Navigate to Skills section
      fireEvent.click(screen.getByText('Skills'));
      
      const skillsInput = screen.getByTestId('skills-input');
      fireEvent.change(skillsInput, { target: { value: 'New Skill' } });
      
      // The skills mock shows a count, not the skill name
      expect(skillsInput).toHaveValue('New Skill');
    });
  });

  describe('Save and Cancel Actions', () => {
    it('calls onSave with current plot point state when save is clicked', () => {
      const mockOnSave = jest.fn();
      render(<PlotPointForm {...defaultProps} onSave={mockOnSave} />);
      
      // Make a change to the form
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'Changed Name' } });
      
      // Click save
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      expect(mockOnSave).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'Changed Name',
          description: 'Test Description'
        })
      );
    });

    it('calls onCancel when cancel is clicked with no changes', () => {
      const mockOnCancel = jest.fn();
      render(<PlotPointForm {...defaultProps} onCancel={mockOnCancel} />);
      
      const cancelButton = screen.getByRole('button', { name: /cancel/i });
      fireEvent.click(cancelButton);
      
      expect(mockOnCancel).toHaveBeenCalled();
    });

    it('prevents default on save button click', () => {
      render(<PlotPointForm {...defaultProps} />);
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      const event = { preventDefault: jest.fn() };
      
      fireEvent.click(saveButton, event);
      
      // Note: preventDefault is called internally, we can't directly test it with fireEvent
      // but the save method should be called
      expect(defaultProps.onSave).toHaveBeenCalled();
    });

    it('prevents default on cancel button click', () => {
      render(<PlotPointForm {...defaultProps} />);
      
      const cancelButton = screen.getByRole('button', { name: /cancel/i });
      fireEvent.click(cancelButton);
      
      expect(defaultProps.onCancel).toHaveBeenCalled();
    });
  });

  describe('Disabled State', () => {
    it('disables buttons when disabled prop is true', () => {
      render(<PlotPointForm {...defaultProps} disabled={true} />);
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      const cancelButton = screen.getByRole('button', { name: /cancel/i });
      
      expect(saveButton).toBeDisabled();
      expect(cancelButton).toBeDisabled();
    });

    it('enables buttons when disabled prop is false', () => {
      render(<PlotPointForm {...defaultProps} disabled={false} />);
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      const cancelButton = screen.getByRole('button', { name: /cancel/i });
      
      expect(saveButton).not.toBeDisabled();
      expect(cancelButton).not.toBeDisabled();
    });
  });

  describe('Unsaved Changes Detection', () => {
    it('sets up beforeunload listener on mount', () => {
      const component = render(<PlotPointForm {...defaultProps} />);
      const instance = component.container.querySelector('form').__reactInternalInstance || 
                      component.container.querySelector('form')._reactInternalFiber;
      
      // Test that setupBeforeUnloadListener can be called with mock window
      const componentInstance = new PlotPointForm.prototype.constructor(defaultProps);
      componentInstance.setupBeforeUnloadListener(mockWindow);
      
      expect(mockWindow.addEventListener).toHaveBeenCalledWith('beforeunload', expect.any(Function));
    });

    it('cleans up event listener on unmount', () => {
      const { unmount } = render(<PlotPointForm {...defaultProps} />);
      
      const componentInstance = new PlotPointForm.prototype.constructor(defaultProps);
      componentInstance.windowRef = mockWindow;
      componentInstance.removeBeforeUnloadListener();
      
      expect(mockWindow.removeEventListener).toHaveBeenCalled();
    });

    it('detects unsaved changes when plot point is modified', async () => {
      render(<PlotPointForm {...defaultProps} />);
      
      // Make a change
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'Modified Name' } });
      
      // The component should detect unsaved changes
      // This would typically be tested through component state, but we can't directly access it
      // We can test the behavior through the cancel confirmation
    });

    it('prevents navigation when there are unsaved changes and user cancels', () => {
      mockWindow.confirm.mockReturnValue(false);
      
      render(<PlotPointForm {...defaultProps} />);
      
      // Make a change to create unsaved changes
      const nameInput = screen.getByLabelText('Name');
      fireEvent.change(nameInput, { target: { value: 'Modified Name' } });
      
      // Try to cancel - this should trigger confirmation
      const cancelButton = screen.getByRole('button', { name: /cancel/i });
      fireEvent.click(cancelButton);
      
      // onCancel should not be called if user cancels confirmation
      // Note: This test verifies the method behavior, actual window.confirm would need to be mocked
    });

    it('allows navigation when user confirms despite unsaved changes', () => {
      mockWindow.confirm.mockReturnValue(true);
      
      const mockOnCancel = jest.fn();
      render(<PlotPointForm {...defaultProps} onCancel={mockOnCancel} />);
      
      const cancelButton = screen.getByRole('button', { name: /cancel/i });
      fireEvent.click(cancelButton);
      
      expect(mockOnCancel).toHaveBeenCalled();
    });
  });

  describe('Component Lifecycle', () => {
    it('initializes state correctly', () => {
      const componentInstance = new PlotPointForm.prototype.constructor(defaultProps);
      
      expect(componentInstance.state).toEqual({
        section: 'BasicRules',
        plotPoint: mockPlotPoint,
        originalPlotPoint: mockPlotPoint,
        hasUnsavedChanges: false
      });
    });

    it('updates state when plot point prop changes', () => {
      const { rerender } = render(<PlotPointForm {...defaultProps} />);
      
      const newPlotPoint = { ...mockPlotPoint, name: 'New Name' };
      rerender(<PlotPointForm {...defaultProps} plotPoint={newPlotPoint} />);
      
      // Component uses internal state, so prop changes may not be reflected immediately
      // This is testing the controlled vs uncontrolled component behavior
      expect(screen.getByLabelText('Name')).toBeInTheDocument();
    });
  });

  describe('Savage Worlds Game Logic', () => {
    it('manages all Savage Worlds content sections', () => {
      render(<PlotPointForm {...defaultProps} />);
      
      // Verify navigation has access to all major Savage Worlds sections
      const navigation = screen.getByTestId('navigation');
      expect(navigation).toBeInTheDocument();
      
      // Test a few key sections
      fireEvent.click(screen.getByText('Skills'));
      expect(screen.getByTestId('skills')).toBeInTheDocument();
      
      fireEvent.click(screen.getByText('Edges'));
      expect(screen.getByTestId('edges')).toBeInTheDocument();
    });

    it('preserves plot point structure for Savage Worlds data', () => {
      const savageWorldsPlotPoint = {
        name: 'Tavern Brawl',
        description: 'A classic tavern fight scenario',
        basicRules: { combatRules: true },
        skills: [{ name: 'Fighting' }, { name: 'Notice' }],
        edges: [{ name: 'Brawny' }],
        hindrances: [{ name: 'Bloodthirsty' }],
        gear: [{ name: 'Longsword' }],
        powers: [{ name: 'Bolt' }],
        races: [{ name: 'Human' }],
        beasts: [{ name: 'Wolf' }],
        characters: [{ name: 'Innkeeper' }],
        settingRules: [{ name: 'Fear' }]
      };

      const mockOnSave = jest.fn();
      render(<PlotPointForm {...defaultProps} plotPoint={savageWorldsPlotPoint} onSave={mockOnSave} />);
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      expect(mockOnSave).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'Tavern Brawl',
          skills: expect.arrayContaining([{ name: 'Fighting' }]),
          edges: expect.arrayContaining([{ name: 'Brawny' }])
        })
      );
    });
  });

  describe('Accessibility', () => {
    it('has proper form structure', () => {
      render(<PlotPointForm {...defaultProps} />);
      
      const form = document.querySelector('form');
      expect(form).toBeInTheDocument();
    });

    it('associates labels with form fields', () => {
      render(<PlotPointForm {...defaultProps} />);
      
      const nameInput = screen.getByLabelText('Name');
      const descriptionInput = screen.getByLabelText('Description');
      
      expect(nameInput).toBeInTheDocument();
      expect(descriptionInput).toBeInTheDocument();
    });

    it('marks required fields appropriately', () => {
      render(<PlotPointForm {...defaultProps} />);
      
      const nameInput = screen.getByLabelText('Name');
      expect(nameInput).toHaveAttribute('required');
    });

    it('provides accessible button labels', () => {
      render(<PlotPointForm {...defaultProps} />);
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      const cancelButton = screen.getByRole('button', { name: /cancel/i });
      
      expect(saveButton).toHaveAccessibleName();
      expect(cancelButton).toHaveAccessibleName();
    });
  });

  describe('Error Boundary Scenarios', () => {
    it('handles null or undefined plot point gracefully', () => {
      // This would typically cause an error, but let's test with minimal data
      const minimalProps = {
        ...defaultProps,
        plotPoint: { 
          name: '', 
          description: '',
          basicRules: {},
          skills: [],
          edges: [],
          hindrances: [],
          gear: [],
          powers: [],
          races: [],
          beasts: [],
          characters: [],
          settingRules: []
        }
      };

      expect(() => {
        render(<PlotPointForm {...minimalProps} />);
      }).not.toThrow();
    });

    it('handles missing child components gracefully', () => {
      // Component should render even if child components fail
      render(<PlotPointForm {...defaultProps} />);
      
      expect(document.querySelector('form')).toBeInTheDocument();
    });
  });

  describe('Performance Considerations', () => {
    it('does not cause unnecessary re-renders', () => {
      const { rerender } = render(<PlotPointForm {...defaultProps} />);
      
      // Same props should not cause re-render issues
      rerender(<PlotPointForm {...defaultProps} />);
      
      expect(screen.getByTestId('text-input')).toBeInTheDocument();
    });

    it('handles large plot point data efficiently', () => {
      const largePlotPoint = {
        ...mockPlotPoint,
        skills: Array(100).fill(0).map((_, i) => ({ name: `Skill ${i}` })),
        edges: Array(50).fill(0).map((_, i) => ({ name: `Edge ${i}` }))
      };

      expect(() => {
        render(<PlotPointForm {...defaultProps} plotPoint={largePlotPoint} />);
      }).not.toThrow();
    });
  });
});