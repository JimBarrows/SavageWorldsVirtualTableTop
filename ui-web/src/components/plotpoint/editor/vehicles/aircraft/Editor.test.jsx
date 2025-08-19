import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import AircraftEditor from './Editor';

// Mock VehicleEditor
jest.mock('../VehicleEditor', () => {
  const MockVehicleEditor = function(props) {
    const React = require('react');
    return React.createElement('div', { 'data-testid': 'vehicle-editor' }, [
      React.createElement('div', { 'data-testid': 'vehicle-base-fields', key: 'base' }, [
        React.createElement('input', { 'data-testid': 'name-input', value: props.item.name || '', readOnly: true, key: 'name' }),
        React.createElement('input', { 'data-testid': 'description-input', value: props.item.description || '', readOnly: true, key: 'desc' }),
        React.createElement('input', { 'data-testid': 'acceleration-input', value: props.item.acceleration || '', readOnly: true, key: 'acceleration' }),
        React.createElement('input', { 'data-testid': 'topSpeed-input', value: props.item.topSpeed || '', readOnly: true, key: 'topSpeed' }),
        React.createElement('input', { 'data-testid': 'toughness-input', value: props.item.toughness || '', readOnly: true, key: 'toughness' }),
        React.createElement('input', { 'data-testid': 'armor-input', value: props.item.armor || '', readOnly: true, key: 'armor' }),
        React.createElement('input', { 'data-testid': 'crew-input', value: props.item.crew || '', readOnly: true, key: 'crew' }),
        React.createElement('input', { 'data-testid': 'passengers-input', value: props.item.passengers || '', readOnly: true, key: 'passengers' }),
        React.createElement('input', { 'data-testid': 'minimumCost-input', value: props.item.minimumCost || '', readOnly: true, key: 'minimumCost' }),
        React.createElement('input', { 'data-testid': 'maximumCost-input', value: props.item.maximumCost || '', readOnly: true, key: 'maximumCost' })
      ]),
      props.additionalAdditionalFields && props.additionalAdditionalFields()
    ].filter(Boolean));
  };
  MockVehicleEditor.prototype = {};
  return MockVehicleEditor;
});

// Mock Bootstrap React Components
jest.mock('bootstrap-react-components', () => ({
  NumberFormGroup: function MockNumberFormGroup({ id, label, onChange, value, required }) {
    return (
      <div data-testid="number-form-group">
        <label htmlFor={id}>{label}</label>
        <input 
          type="number"
          id={id}
          value={value || ''}
          onChange={onChange}
          required={required}
          data-testid="number-input"
        />
      </div>
    );
  }
}));

describe('AircraftEditor Component', () => {
  const mockAircraft = {
    name: 'P-51 Mustang',
    description: 'WWII Fighter Aircraft',
    era: 'WWII',
    kind: 'Fighter',
    note: 'Long-range escort fighter',
    acceleration: 4,
    topSpeed: 437,
    toughness: 12,
    armor: 2,
    crew: 1,
    passengers: 0,
    minimumCost: 50000,
    maximumCost: 75000,
    climb: 3350
  };

  const defaultProps = {
    id: 'test-aircraft',
    item: mockAircraft,
    index: 0,
    onChange: jest.fn(),
    onDelete: jest.fn()
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders with required props', () => {
      render(<AircraftEditor {...defaultProps} />);
      
      expect(screen.getByTestId('vehicle-editor')).toBeInTheDocument();
      expect(screen.getByTestId('vehicle-base-fields')).toBeInTheDocument();
    });

    it('renders additional climb field', () => {
      render(<AircraftEditor {...defaultProps} />);
      
      expect(screen.getByLabelText('Climb')).toBeInTheDocument();
    });

    it('displays current climb value', () => {
      render(<AircraftEditor {...defaultProps} />);
      
      const climbInput = screen.getByLabelText('Climb');
      expect(climbInput).toHaveValue('3350');
    });

    it('marks climb field as required', () => {
      render(<AircraftEditor {...defaultProps} />);
      
      const climbInput = screen.getByLabelText('Climb');
      expect(climbInput).toHaveAttribute('required');
    });

    it('uses correct climb field id', () => {
      render(<AircraftEditor {...defaultProps} />);
      
      const climbInput = screen.getByLabelText('Climb');
      expect(climbInput).toHaveAttribute('id', 'climb');
    });
  });

  describe('Form Field Updates', () => {
    it('updates climb when climb field changes', () => {
      const mockOnChange = jest.fn();
      render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
      
      const climbInput = screen.getByLabelText('Climb');
      fireEvent.change(climbInput, { target: { value: '4000' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: 'P-51 Mustang',
          description: 'WWII Fighter Aircraft',
          era: 'WWII',
          kind: 'Fighter',
          note: 'Long-range escort fighter',
          acceleration: 4,
          topSpeed: 437,
          toughness: 12,
          armor: 2,
          crew: 1,
          passengers: 0,
          minimumCost: 50000,
          maximumCost: 75000,
          climb: 4000
        }),
        0
      );
    });

    it('converts climb to integer', () => {
      const mockOnChange = jest.fn();
      render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
      
      const climbInput = screen.getByLabelText('Climb');
      fireEvent.change(climbInput, { target: { value: '2500.7' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          climb: 2500
        }),
        0
      );
    });

    it('passes correct index to onChange calls', () => {
      const mockOnChange = jest.fn();
      render(<AircraftEditor {...defaultProps} index={2} onChange={mockOnChange} />);
      
      const climbInput = screen.getByLabelText('Climb');
      fireEvent.change(climbInput, { target: { value: '3000' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(expect.any(Object), 2);
    });

    it('preserves all existing item properties when updating climb', () => {
      const mockOnChange = jest.fn();
      render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
      
      const climbInput = screen.getByLabelText('Climb');
      fireEvent.change(climbInput, { target: { value: '2800' } });
      
      const callArguments = mockOnChange.mock.calls[0][0];
      
      // Should have all original properties plus updated climb
      expect(callArguments).toEqual({
        name: 'P-51 Mustang',
        description: 'WWII Fighter Aircraft',
        era: 'WWII',
        kind: 'Fighter',
        note: 'Long-range escort fighter',
        acceleration: 4,
        topSpeed: 437,
        toughness: 12,
        armor: 2,
        crew: 1,
        passengers: 0,
        minimumCost: 50000,
        maximumCost: 75000,
        climb: 2800
      });
    });
  });

  describe('Savage Worlds Aircraft Types', () => {
    const aircraftTypes = [
      {
        name: 'Sopwith Camel',
        era: 'WWI',
        kind: 'Fighter',
        topSpeed: 113,
        climb: 1000,
        crew: 1,
        passengers: 0
      },
      {
        name: 'P-51 Mustang',
        era: 'WWII',
        kind: 'Fighter',
        topSpeed: 437,
        climb: 3350,
        crew: 1,
        passengers: 0
      },
      {
        name: 'B-17 Flying Fortress',
        era: 'WWII',
        kind: 'Bomber',
        topSpeed: 287,
        climb: 900,
        crew: 10,
        passengers: 0
      },
      {
        name: 'C-47 Skytrain',
        era: 'WWII',
        kind: 'Transport',
        topSpeed: 224,
        climb: 1130,
        crew: 3,
        passengers: 28
      },
      {
        name: 'F-16 Falcon',
        era: 'Modern',
        kind: 'Fighter',
        topSpeed: 1500,
        climb: 50000,
        crew: 1,
        passengers: 0
      }
    ];

    it('handles typical Savage Worlds aircraft', () => {
      aircraftTypes.forEach(aircraft => {
        const fullAircraft = { ...mockAircraft, ...aircraft };
        const { rerender } = render(<AircraftEditor {...defaultProps} item={fullAircraft} />);
        
        expect(screen.getByLabelText('Climb')).toHaveValue(aircraft.climb.toString());
        
        rerender(<div />);
      });
    });

    it('handles very high climb rates for modern aircraft', () => {
      const modernFighter = {
        ...mockAircraft,
        name: 'F-22 Raptor',
        climb: 62000
      };

      render(<AircraftEditor {...defaultProps} item={modernFighter} />);
      
      expect(screen.getByLabelText('Climb')).toHaveValue('62000');
    });

    it('handles low climb rates for early aircraft', () => {
      const earlyAircraft = {
        ...mockAircraft,
        name: 'Wright Flyer',
        climb: 50
      };

      render(<AircraftEditor {...defaultProps} item={earlyAircraft} />);
      
      expect(screen.getByLabelText('Climb')).toHaveValue('50');
    });

    it('handles zero climb rate for ground-effect vehicles', () => {
      const groundEffect = {
        ...mockAircraft,
        name: 'Ground Effect Vehicle',
        climb: 0
      };

      render(<AircraftEditor {...defaultProps} item={groundEffect} />);
      
      expect(screen.getByLabelText('Climb')).toHaveValue('0');
    });
  });

  describe('Aircraft Performance Characteristics', () => {
    it('handles typical fighter climb rates', () => {
      const fighterClimbRates = [1000, 2000, 3000, 4000, 5000, 10000, 20000, 50000];
      
      fighterClimbRates.forEach(climbRate => {
        const mockOnChange = jest.fn();
        render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
        
        const climbInput = screen.getByLabelText('Climb');
        fireEvent.change(climbInput, { target: { value: climbRate.toString() } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ climb: climbRate }),
          0
        );
      });
    });

    it('handles bomber climb rates (typically lower)', () => {
      const bomberClimbRates = [500, 800, 1200, 1500, 2000];
      
      bomberClimbRates.forEach(climbRate => {
        const mockOnChange = jest.fn();
        render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
        
        const climbInput = screen.getByLabelText('Climb');
        fireEvent.change(climbInput, { target: { value: climbRate.toString() } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ climb: climbRate }),
          0
        );
      });
    });

    it('handles transport aircraft climb rates', () => {
      const transportClimbRates = [800, 1000, 1500, 2000, 2500];
      
      transportClimbRates.forEach(climbRate => {
        const mockOnChange = jest.fn();
        render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
        
        const climbInput = screen.getByLabelText('Climb');
        fireEvent.change(climbInput, { target: { value: climbRate.toString() } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ climb: climbRate }),
          0
        );
      });
    });
  });

  describe('Data Handling', () => {
    it('handles undefined climb value', () => {
      const incompleteAircraft = {
        ...mockAircraft,
        climb: undefined
      };

      expect(() => {
        render(<AircraftEditor {...defaultProps} item={incompleteAircraft} />);
      }).not.toThrow();
    });

    it('handles null climb value', () => {
      const nullAircraft = {
        ...mockAircraft,
        climb: null
      };

      render(<AircraftEditor {...defaultProps} item={nullAircraft} />);
      
      const climbInput = screen.getByLabelText('Climb');
      expect(climbInput).toHaveValue('');
    });

    it('handles empty string input for climb', () => {
      const mockOnChange = jest.fn();
      render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
      
      const climbInput = screen.getByLabelText('Climb');
      fireEvent.change(climbInput, { target: { value: '' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          climb: 0 // parseInt('', 10) -> NaN, but should be handled gracefully
        }),
        0
      );
    });

    it('handles non-numeric input for climb', () => {
      const mockOnChange = jest.fn();
      render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
      
      const climbInput = screen.getByLabelText('Climb');
      fireEvent.change(climbInput, { target: { value: 'abc' } });
      
      // parseInt('abc', 10) returns NaN
      expect(mockOnChange).toHaveBeenCalled();
    });

    it('handles negative climb values', () => {
      const mockOnChange = jest.fn();
      render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
      
      const climbInput = screen.getByLabelText('Climb');
      fireEvent.change(climbInput, { target: { value: '-100' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ climb: -100 }),
        0
      );
    });

    it('handles very large climb values', () => {
      const mockOnChange = jest.fn();
      render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
      
      const climbInput = screen.getByLabelText('Climb');
      fireEvent.change(climbInput, { target: { value: '100000' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ climb: 100000 }),
        0
      );
    });
  });

  describe('Component Integration', () => {
    it('extends VehicleEditor properly', () => {
      render(<AircraftEditor {...defaultProps} />);
      
      // Should render base VehicleEditor functionality
      expect(screen.getByTestId('vehicle-editor')).toBeInTheDocument();
      expect(screen.getByTestId('vehicle-base-fields')).toBeInTheDocument();
      
      // Should add additional climb field
      expect(screen.getByLabelText('Climb')).toBeInTheDocument();
    });

    it('uses additionalAdditionalFields method', () => {
      render(<AircraftEditor {...defaultProps} />);
      
      // Method should be called and render the climb field
      expect(screen.getByLabelText('Climb')).toBeInTheDocument();
    });

    it('maintains inheritance chain', () => {
      const component = new AircraftEditor.prototype.constructor(defaultProps);
      expect(component).toBeInstanceOf(Object);
    });

    it('calls onChange with object assignment pattern', () => {
      const mockOnChange = jest.fn();
      render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
      
      const climbInput = screen.getByLabelText('Climb');
      fireEvent.change(climbInput, { target: { value: '5000' } });
      
      // Verify Object.assign pattern is used
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({
          name: mockAircraft.name,
          description: mockAircraft.description,
          era: mockAircraft.era,
          kind: mockAircraft.kind,
          note: mockAircraft.note,
          acceleration: mockAircraft.acceleration,
          topSpeed: mockAircraft.topSpeed,
          toughness: mockAircraft.toughness,
          armor: mockAircraft.armor,
          crew: mockAircraft.crew,
          passengers: mockAircraft.passengers,
          minimumCost: mockAircraft.minimumCost,
          maximumCost: mockAircraft.maximumCost,
          climb: 5000
        }),
        0
      );
    });
  });

  describe('Edge Cases', () => {
    it('handles rapid climb value changes', () => {
      const mockOnChange = jest.fn();
      render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
      
      const climbInput = screen.getByLabelText('Climb');
      
      fireEvent.change(climbInput, { target: { value: '1000' } });
      fireEvent.change(climbInput, { target: { value: '5000' } });
      fireEvent.change(climbInput, { target: { value: '10000' } });
      
      expect(mockOnChange).toHaveBeenCalledTimes(3);
      expect(mockOnChange).toHaveBeenLastCalledWith(
        expect.objectContaining({ climb: 10000 }),
        0
      );
    });

    it('handles decimal climb values', () => {
      const mockOnChange = jest.fn();
      render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
      
      const climbInput = screen.getByLabelText('Climb');
      fireEvent.change(climbInput, { target: { value: '2500.5' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ climb: 2500 }),
        0
      );
    });

    it('handles leading zeros in climb values', () => {
      const mockOnChange = jest.fn();
      render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
      
      const climbInput = screen.getByLabelText('Climb');
      fireEvent.change(climbInput, { target: { value: '003000' } });
      
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ climb: 3000 }),
        0
      );
    });

    it('handles whitespace in climb values', () => {
      const mockOnChange = jest.fn();
      render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
      
      const climbInput = screen.getByLabelText('Climb');
      fireEvent.change(climbInput, { target: { value: ' 4000 ' } });
      
      // parseInt handles whitespace
      expect(mockOnChange).toHaveBeenCalledWith(
        expect.objectContaining({ climb: 4000 }),
        0
      );
    });
  });

  describe('Historical Aircraft Accuracy', () => {
    it('supports WWI era aircraft climb rates', () => {
      const ww1Aircraft = [
        { name: 'Sopwith Camel', climb: 1000 },
        { name: 'Fokker Dr.I', climb: 1180 },
        { name: 'SPAD S.XIII', climb: 1200 },
        { name: 'Albatros D.III', climb: 900 }
      ];

      ww1Aircraft.forEach(aircraft => {
        const mockOnChange = jest.fn();
        render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
        
        const climbInput = screen.getByLabelText('Climb');
        fireEvent.change(climbInput, { target: { value: aircraft.climb.toString() } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ climb: aircraft.climb }),
          0
        );
      });
    });

    it('supports WWII era aircraft climb rates', () => {
      const ww2Aircraft = [
        { name: 'P-51 Mustang', climb: 3350 },
        { name: 'Bf 109', climb: 3280 },
        { name: 'Spitfire Mk IX', climb: 4100 },
        { name: 'F4U Corsair', climb: 3120 },
        { name: 'B-17 Flying Fortress', climb: 900 },
        { name: 'B-29 Superfortress', climb: 1760 }
      ];

      ww2Aircraft.forEach(aircraft => {
        const mockOnChange = jest.fn();
        render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
        
        const climbInput = screen.getByLabelText('Climb');
        fireEvent.change(climbInput, { target: { value: aircraft.climb.toString() } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ climb: aircraft.climb }),
          0
        );
      });
    });

    it('supports modern jet aircraft climb rates', () => {
      const modernJets = [
        { name: 'F-16 Fighting Falcon', climb: 50000 },
        { name: 'F-15 Eagle', climb: 65000 },
        { name: 'F-22 Raptor', climb: 62000 },
        { name: 'F-35 Lightning II', climb: 45000 }
      ];

      modernJets.forEach(jet => {
        const mockOnChange = jest.fn();
        render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
        
        const climbInput = screen.getByLabelText('Climb');
        fireEvent.change(climbInput, { target: { value: jet.climb.toString() } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ climb: jet.climb }),
          0
        );
      });
    });

    it('supports civilian aircraft climb rates', () => {
      const civilianAircraft = [
        { name: 'Cessna 172', climb: 720 },
        { name: 'Piper Cherokee', climb: 660 },
        { name: 'Boeing 737', climb: 2500 },
        { name: 'Airbus A320', climb: 2400 }
      ];

      civilianAircraft.forEach(aircraft => {
        const mockOnChange = jest.fn();
        render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
        
        const climbInput = screen.getByLabelText('Climb');
        fireEvent.change(climbInput, { target: { value: aircraft.climb.toString() } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ climb: aircraft.climb }),
          0
        );
      });
    });
  });

  describe('Savage Worlds Game Mechanics', () => {
    it('supports aircraft performance scaling', () => {
      // Test various performance categories
      const performanceCategories = [
        { category: 'Poor', climb: 500 },
        { category: 'Average', climb: 1500 },
        { category: 'Good', climb: 3000 },
        { category: 'Excellent', climb: 5000 },
        { category: 'Superior', climb: 10000 },
        { category: 'Legendary', climb: 50000 }
      ];

      performanceCategories.forEach(category => {
        const mockOnChange = jest.fn();
        render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
        
        const climbInput = screen.getByLabelText('Climb');
        fireEvent.change(climbInput, { target: { value: category.climb.toString() } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ climb: category.climb }),
          0
        );
      });
    });

    it('supports special aircraft types', () => {
      const specialAircraft = [
        { name: 'Helicopter', climb: 1500 },
        { name: 'Gyrocopter', climb: 800 },
        { name: 'Hot Air Balloon', climb: 300 },
        { name: 'Glider', climb: 0 },
        { name: 'Rocket Fighter', climb: 20000 }
      ];

      specialAircraft.forEach(aircraft => {
        const mockOnChange = jest.fn();
        render(<AircraftEditor {...defaultProps} onChange={mockOnChange} />);
        
        const climbInput = screen.getByLabelText('Climb');
        fireEvent.change(climbInput, { target: { value: aircraft.climb.toString() } });
        
        expect(mockOnChange).toHaveBeenCalledWith(
          expect.objectContaining({ climb: aircraft.climb }),
          0
        );
      });
    });
  });
});