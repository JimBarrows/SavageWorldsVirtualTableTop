import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import VehicleEditor from './VehicleEditor';

// Mock GearEditor
jest.mock('../gear/GearEditor', () => {
  const { Component } = require('react');
  
  return class MockGearEditor extends Component {
    static propTypes = {};
    
    render() {
      const { id, item, onChange, onDelete, index } = this.props;
      const componentId = `GearEditor-${id}`;
      
      return (
        <div data-testid="gear-editor" data-id={componentId}>
          <div data-testid="gear-base-fields">
            <input
              data-testid="gear-name"
              value={item.name || ''}
              onChange={(e) => onChange({ ...item, name: e.target.value }, index)}
            />
            <textarea
              data-testid="gear-description"
              value={item.description || ''}
              onChange={(e) => onChange({ ...item, description: e.target.value }, index)}
            />
            <input
              data-testid="gear-era"
              value={item.era || ''}
              onChange={(e) => onChange({ ...item, era: e.target.value }, index)}
            />
            <input
              data-testid="gear-kind"
              value={item.kind || ''}
              onChange={(e) => onChange({ ...item, kind: e.target.value }, index)}
            />
            <textarea
              data-testid="gear-note"
              value={item.note || ''}
              onChange={(e) => onChange({ ...item, note: e.target.value }, index)}
            />
          </div>
          {this.additionalFields && this.additionalFields()}
          <button data-testid="delete-button" onClick={onDelete}>Delete</button>
        </div>
      );
    }
  };
});

// Mock Bootstrap React Components
jest.mock('bootstrap-react-components', () => ({
  NumberFormGroup: function MockNumberFormGroup({ id, label, onChange, value, required }) {
    return (
      <div data-testid={`number-form-group-${id}`}>
        <label htmlFor={id}>{label}</label>
        <input 
          id={id}
          type="number"
          value={value !== undefined && value !== null ? value : ''}
          onChange={onChange}
          required={required}
          data-testid={`number-input-${id}`}
        />
      </div>
    );
  }
}));

describe('VehicleEditor Component', () => {
  const mockVehicleItem = {
    name: 'Sports Car',
    description: 'Fast and agile vehicle',
    era: 'Modern',
    kind: 'Car',
    note: 'High performance vehicle',
    acceleration: 20,
    armor: 2,
    crew: 1,
    passengers: 1,
    topSpeed: 120,
    toughness: 9,
    minimumCost: 25000,
    maximumCost: 50000
  };

  const defaultProps = {
    item: mockVehicleItem,
    onChange: jest.fn(),
    onDelete: jest.fn(),
    id: 'test-vehicle',
    index: 0
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with all vehicle-specific fields', () => {
      render(<VehicleEditor {...defaultProps} />);
      
      expect(screen.getByTestId('gear-editor')).toBeInTheDocument();
      expect(screen.getByLabelText('Acceleration')).toBeInTheDocument();
      expect(screen.getByLabelText('Top Speed')).toBeInTheDocument();
      expect(screen.getByLabelText('Toughness')).toBeInTheDocument();
      expect(screen.getByLabelText('Armor')).toBeInTheDocument();
      expect(screen.getByLabelText('Crew')).toBeInTheDocument();
      expect(screen.getByLabelText('Passengers')).toBeInTheDocument();
      expect(screen.getByLabelText('Minimum Cost')).toBeInTheDocument();
      expect(screen.getByLabelText('Maximum Cost')).toBeInTheDocument();
    });

    it('displays correct initial values for all vehicle fields', () => {
      render(<VehicleEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Acceleration')).toHaveValue(20);
      expect(screen.getByLabelText('Top Speed')).toHaveValue(120);
      expect(screen.getByLabelText('Toughness')).toHaveValue(9);
      expect(screen.getByLabelText('Armor')).toHaveValue(2);
      expect(screen.getByLabelText('Crew')).toHaveValue(1);
      expect(screen.getByLabelText('Passengers')).toHaveValue(1);
      expect(screen.getByLabelText('Minimum Cost')).toHaveValue(25000);
      expect(screen.getByLabelText('Maximum Cost')).toHaveValue(50000);
    });

    it('marks all vehicle fields as required', () => {
      render(<VehicleEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Acceleration')).toHaveAttribute('required');
      expect(screen.getByLabelText('Top Speed')).toHaveAttribute('required');
      expect(screen.getByLabelText('Toughness')).toHaveAttribute('required');
      expect(screen.getByLabelText('Armor')).toHaveAttribute('required');
      expect(screen.getByLabelText('Crew')).toHaveAttribute('required');
      expect(screen.getByLabelText('Passengers')).toHaveAttribute('required');
      expect(screen.getByLabelText('Minimum Cost')).toHaveAttribute('required');
      expect(screen.getByLabelText('Maximum Cost')).toHaveAttribute('required');
    });

    it('renders with correct NumberFormGroup IDs', () => {
      render(<VehicleEditor {...defaultProps} />);
      
      expect(screen.getByTestId('number-form-group-BaseVehicle-Acceleration')).toBeInTheDocument();
      expect(screen.getByTestId('number-form-group-BaseVehicle-TopSpeed')).toBeInTheDocument();
      expect(screen.getByTestId('number-form-group-BaseVehicle-Toughness')).toBeInTheDocument();
      expect(screen.getByTestId('number-form-group-BaseVehicle-Armor')).toBeInTheDocument();
      expect(screen.getByTestId('number-form-group-BaseVehicle-Crew')).toBeInTheDocument();
      expect(screen.getByTestId('number-form-group-BaseVehicle-Passengers')).toBeInTheDocument();
      expect(screen.getByTestId('number-form-group-BaseVehicle-MinimumCostChange')).toBeInTheDocument();
      expect(screen.getByTestId('number-form-group-BaseVehicle-MaximumCostChange')).toBeInTheDocument();
    });
  });

  describe('Inheritance from GearEditor', () => {
    it('extends GearEditor and renders base gear fields', () => {
      render(<VehicleEditor {...defaultProps} />);
      
      expect(screen.getByTestId('gear-base-fields')).toBeInTheDocument();
      expect(screen.getByTestId('gear-name')).toHaveValue('Sports Car');
      expect(screen.getByTestId('gear-description')).toHaveValue('Fast and agile vehicle');
      expect(screen.getByTestId('gear-era')).toHaveValue('Modern');
      expect(screen.getByTestId('gear-kind')).toHaveValue('Car');
      expect(screen.getByTestId('gear-note')).toHaveValue('High performance vehicle');
    });

    it('inherits delete functionality from GearEditor', () => {
      const mockOnDelete = jest.fn();
      render(<VehicleEditor {...defaultProps} onDelete={mockOnDelete} />);
      
      const deleteButton = screen.getByTestId('delete-button');
      fireEvent.click(deleteButton);
      
      expect(mockOnDelete).toHaveBeenCalledTimes(1);
    });

    it('passes correct props to parent GearEditor', () => {
      render(<VehicleEditor {...defaultProps} id="custom-vehicle" index={5} />);
      
      const gearEditor = screen.getByTestId('gear-editor');
      expect(gearEditor).toHaveAttribute('data-id', 'GearEditor-custom-vehicle');
    });
  });

  describe('Numeric Field Change Handlers', () => {
    describe('Acceleration Field', () => {
      it('updates acceleration value correctly', () => {
        const mockOnChange = jest.fn();
        render(<VehicleEditor {...defaultProps} onChange={mockOnChange} />);
        
        const accelerationInput = screen.getByLabelText('Acceleration');
        fireEvent.change(accelerationInput, { target: { value: '25' } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({
            ...mockVehicleItem,
            acceleration: 25
          }),
          0
        );
      });

      it('parses acceleration as integer', () => {
        const mockOnChange = jest.fn();
        render(<VehicleEditor {...defaultProps} onChange={mockOnChange} />);
        
        const accelerationInput = screen.getByLabelText('Acceleration');
        fireEvent.change(accelerationInput, { target: { value: '15.7' } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({
            acceleration: 15
          }),
          0
        );
      });

      it('handles invalid acceleration input', () => {
        const mockOnChange = jest.fn();
        render(<VehicleEditor {...defaultProps} onChange={mockOnChange} />);
        
        const accelerationInput = screen.getByLabelText('Acceleration');
        fireEvent.change(accelerationInput, { target: { value: 'invalid' } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({
            acceleration: NaN
          }),
          0
        );
      });
    });

    describe('Armor Field', () => {
      it('updates armor value correctly', () => {
        const mockOnChange = jest.fn();
        render(<VehicleEditor {...defaultProps} onChange={mockOnChange} />);
        
        const armorInput = screen.getByLabelText('Armor');
        fireEvent.change(armorInput, { target: { value: '4' } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({
            armor: 4
          }),
          0
        );
      });

      it('handles zero armor value', () => {
        const mockOnChange = jest.fn();
        render(<VehicleEditor {...defaultProps} onChange={mockOnChange} />);
        
        const armorInput = screen.getByLabelText('Armor');
        fireEvent.change(armorInput, { target: { value: '0' } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({
            armor: 0
          }),
          0
        );
      });
    });

    describe('Crew Field', () => {
      it('updates crew value correctly', () => {
        const mockOnChange = jest.fn();
        render(<VehicleEditor {...defaultProps} onChange={mockOnChange} />);
        
        const crewInput = screen.getByLabelText('Crew');
        fireEvent.change(crewInput, { target: { value: '3' } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({
            crew: 3
          }),
          0
        );
      });
    });

    describe('Passengers Field', () => {
      it('updates passengers value correctly', () => {
        const mockOnChange = jest.fn();
        render(<VehicleEditor {...defaultProps} onChange={mockOnChange} />);
        
        const passengersInput = screen.getByLabelText('Passengers');
        fireEvent.change(passengersInput, { target: { value: '4' } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({
            passengers: 4
          }),
          0
        );
      });
    });

    describe('Top Speed Field', () => {
      it('updates top speed value correctly', () => {
        const mockOnChange = jest.fn();
        render(<VehicleEditor {...defaultProps} onChange={mockOnChange} />);
        
        const topSpeedInput = screen.getByLabelText('Top Speed');
        fireEvent.change(topSpeedInput, { target: { value: '150' } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({
            topSpeed: 150
          }),
          0
        );
      });
    });

    describe('Toughness Field', () => {
      it('updates toughness value correctly', () => {
        const mockOnChange = jest.fn();
        render(<VehicleEditor {...defaultProps} onChange={mockOnChange} />);
        
        const toughnessInput = screen.getByLabelText('Toughness');
        fireEvent.change(toughnessInput, { target: { value: '12' } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({
            toughness: 12
          }),
          0
        );
      });
    });

    describe('Cost Fields', () => {
      it('updates minimum cost correctly', () => {
        const mockOnChange = jest.fn();
        render(<VehicleEditor {...defaultProps} onChange={mockOnChange} />);
        
        const minCostInput = screen.getByLabelText('Minimum Cost');
        fireEvent.change(minCostInput, { target: { value: '30000' } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({
            minimumCost: 30000
          }),
          0
        );
      });

      it('updates maximum cost correctly', () => {
        const mockOnChange = jest.fn();
        render(<VehicleEditor {...defaultProps} onChange={mockOnChange} />);
        
        const maxCostInput = screen.getByLabelText('Maximum Cost');
        fireEvent.change(maxCostInput, { target: { value: '75000' } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({
            maximumCost: 75000
          }),
          0
        );
      });
    });
  });

  describe('Data Preservation', () => {
    it('preserves all unchanged fields when updating single field', () => {
      const mockOnChange = jest.fn();
      render(<VehicleEditor {...defaultProps} onChange={mockOnChange} />);
      
      const accelerationInput = screen.getByLabelText('Acceleration');
      fireEvent.change(accelerationInput, { target: { value: '30' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        {
          name: 'Sports Car',
          description: 'Fast and agile vehicle',
          era: 'Modern',
          kind: 'Car',
          note: 'High performance vehicle',
          acceleration: 30,
          armor: 2,
          crew: 1,
          passengers: 1,
          topSpeed: 120,
          toughness: 9,
          minimumCost: 25000,
          maximumCost: 50000
        },
        0
      );
    });

    it('passes correct index to onChange', () => {
      const mockOnChange = jest.fn();
      render(<VehicleEditor {...defaultProps} index={7} onChange={mockOnChange} />);
      
      const accelerationInput = screen.getByLabelText('Acceleration');
      fireEvent.change(accelerationInput, { target: { value: '25' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(expect.any(Object), 7);
    });
  });

  describe('Savage Worlds Vehicle Types', () => {
    it('handles car vehicles correctly', () => {
      const carVehicle = {
        ...mockVehicleItem,
        name: 'Ford Mustang',
        kind: 'Car',
        acceleration: 20,
        topSpeed: 120,
        toughness: 9,
        armor: 2,
        crew: 1,
        passengers: 3
      };

      render(<VehicleEditor {...defaultProps} item={carVehicle} />);
      
      expect(screen.getByTestId('gear-name')).toHaveValue('Ford Mustang');
      expect(screen.getByTestId('gear-kind')).toHaveValue('Car');
      expect(screen.getByLabelText('Acceleration')).toHaveValue(20);
      expect(screen.getByLabelText('Top Speed')).toHaveValue(120);
      expect(screen.getByLabelText('Passengers')).toHaveValue(3);
    });

    it('handles aircraft vehicles correctly', () => {
      const aircraftVehicle = {
        ...mockVehicleItem,
        name: 'Cessna 172',
        kind: 'Aircraft',
        acceleration: 10,
        topSpeed: 140,
        toughness: 8,
        armor: 0,
        crew: 1,
        passengers: 3,
        minimumCost: 200000,
        maximumCost: 350000
      };

      render(<VehicleEditor {...defaultProps} item={aircraftVehicle} />);
      
      expect(screen.getByTestId('gear-name')).toHaveValue('Cessna 172');
      expect(screen.getByTestId('gear-kind')).toHaveValue('Aircraft');
      expect(screen.getByLabelText('Top Speed')).toHaveValue(140);
      expect(screen.getByLabelText('Armor')).toHaveValue(0);
      expect(screen.getByLabelText('Maximum Cost')).toHaveValue(350000);
    });

    it('handles boat vehicles correctly', () => {
      const boatVehicle = {
        ...mockVehicleItem,
        name: 'Speedboat',
        kind: 'Boat',
        acceleration: 15,
        topSpeed: 60,
        toughness: 10,
        armor: 1,
        crew: 1,
        passengers: 6,
        minimumCost: 50000,
        maximumCost: 100000
      };

      render(<VehicleEditor {...defaultProps} item={boatVehicle} />);
      
      expect(screen.getByTestId('gear-name')).toHaveValue('Speedboat');
      expect(screen.getByTestId('gear-kind')).toHaveValue('Boat');
      expect(screen.getByLabelText('Top Speed')).toHaveValue(60);
      expect(screen.getByLabelText('Passengers')).toHaveValue(6);
    });

    it('handles military vehicle correctly', () => {
      const militaryVehicle = {
        ...mockVehicleItem,
        name: 'Tank',
        kind: 'Military',
        acceleration: 5,
        topSpeed: 40,
        toughness: 20,
        armor: 10,
        crew: 4,
        passengers: 0,
        minimumCost: 2000000,
        maximumCost: 5000000
      };

      render(<VehicleEditor {...defaultProps} item={militaryVehicle} />);
      
      expect(screen.getByTestId('gear-name')).toHaveValue('Tank');
      expect(screen.getByTestId('gear-kind')).toHaveValue('Military');
      expect(screen.getByLabelText('Acceleration')).toHaveValue(5);
      expect(screen.getByLabelText('Toughness')).toHaveValue(20);
      expect(screen.getByLabelText('Armor')).toHaveValue(10);
      expect(screen.getByLabelText('Crew')).toHaveValue(4);
      expect(screen.getByLabelText('Passengers')).toHaveValue(0);
    });
  });

  describe('Edge Cases and Error Handling', () => {
    it('handles empty vehicle item gracefully', () => {
      const emptyVehicle = {};
      
      expect(() => {
        render(<VehicleEditor {...defaultProps} item={emptyVehicle} />);
      }).not.toThrow();
    });

    it('handles missing numeric properties', () => {
      const incompleteVehicle = {
        name: 'Incomplete Vehicle',
        description: 'Missing some properties'
      };

      expect(() => {
        render(<VehicleEditor {...defaultProps} item={incompleteVehicle} />);
      }).not.toThrow();
    });

    it('handles negative values correctly', () => {
      const mockOnChange = jest.fn();
      render(<VehicleEditor {...defaultProps} onChange={mockOnChange} />);
      
      const accelerationInput = screen.getByLabelText('Acceleration');
      fireEvent.change(accelerationInput, { target: { value: '-5' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          acceleration: -5
        }),
        0
      );
    });

    it('handles very large numbers', () => {
      const mockOnChange = jest.fn();
      render(<VehicleEditor {...defaultProps} onChange={mockOnChange} />);
      
      const maxCostInput = screen.getByLabelText('Maximum Cost');
      fireEvent.change(maxCostInput, { target: { value: '999999999' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          maximumCost: 999999999
        }),
        0
      );
    });

    it('handles empty string input', () => {
      const mockOnChange = jest.fn();
      render(<VehicleEditor {...defaultProps} onChange={mockOnChange} />);
      
      const accelerationInput = screen.getByLabelText('Acceleration');
      fireEvent.change(accelerationInput, { target: { value: '' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          acceleration: NaN
        }),
        0
      );
    });
  });

  describe('Integration with additionalAdditionalFields', () => {
    it('renders additionalAdditionalFields when present', () => {
      class ExtendedVehicleEditor extends VehicleEditor {
        additionalAdditionalFields = () => (
          <div data-testid="additional-additional-fields">
            <input data-testid="extra-field" />
          </div>
        );
      }

      render(<ExtendedVehicleEditor {...defaultProps} />);
      
      expect(screen.getByTestId('additional-additional-fields')).toBeInTheDocument();
      expect(screen.getByTestId('extra-field')).toBeInTheDocument();
    });

    it('does not render additionalAdditionalFields when not present', () => {
      render(<VehicleEditor {...defaultProps} />);
      
      expect(screen.queryByTestId('additional-additional-fields')).not.toBeInTheDocument();
    });
  });

  describe('PropTypes Validation', () => {
    it('accepts all required propTypes', () => {
      const validProps = {
        item: {
          acceleration: 20,
          armor: 2,
          crew: 1,
          description: 'Test vehicle',
          era: 'Modern',
          kind: 'Car',
          maximumCost: 50000,
          minimumCost: 25000,
          name: 'Test Car',
          note: 'Test note',
          passengers: 4,
          topSpeed: 120,
          toughness: 9
        },
        onChange: jest.fn()
      };

      expect(() => {
        render(<VehicleEditor {...validProps} />);
      }).not.toThrow();
    });
  });

  describe('State Management Integration', () => {
    it('maintains consistency across multiple field updates', () => {
      const mockOnChange = jest.fn();
      render(<VehicleEditor {...defaultProps} onChange={mockOnChange} />);
      
      // Update multiple fields
      const accelerationInput = screen.getByLabelText('Acceleration');
      const topSpeedInput = screen.getByLabelText('Top Speed');
      const armorInput = screen.getByLabelText('Armor');
      
      fireEvent.change(accelerationInput, { target: { value: '25' } });
      fireEvent.change(topSpeedInput, { target: { value: '130' } });
      fireEvent.change(armorInput, { target: { value: '3' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(3);
      
      // Each call should preserve all other fields
      expect(mockOnChange).toHaveBeenNthCalledWith(1, 
        expect.objectContaining({
          acceleration: 25,
          armor: 2, // original value
          topSpeed: 120 // original value
        }),
        0
      );
      
      expect(mockOnChange).toHaveBeenNthCalledWith(2,
        expect.objectContaining({
          acceleration: 20, // original value  
          topSpeed: 130,
          armor: 2 // original value
        }),
        0
      );
      
      expect(mockOnChange).toHaveBeenNthCalledWith(3,
        expect.objectContaining({
          acceleration: 20, // original value
          topSpeed: 120, // original value
          armor: 3
        }),
        0
      );
    });
  });
});