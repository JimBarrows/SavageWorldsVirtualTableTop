import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import EditorList from './EditorList';

// Mock child component for testing
const MockItemEditor = ({ item, index, onChange, onDelete }) => (
  <div>
    <span>{item.name}</span>
    <button onClick={() => onChange({ ...item, name: 'Modified' }, index)}>
      Edit
    </button>
    <button onClick={() => onDelete(index)}>Delete</button>
  </div>
);

describe('EditorList Component', () => {
  const defaultProps = {
    id: 'test-list',
    title: 'Test Items',
    list: [
      { id: '1', name: 'Item 1' },
      { id: '2', name: 'Item 2' },
      { id: '3', name: 'Item 3' }
    ],
    emptyItem: { id: '', name: '' },
    onChange: jest.fn(),
    children: <MockItemEditor />
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders without crashing', () => {
      render(<EditorList {...defaultProps} />);
    });

    it('displays the title as h2 by default', () => {
      render(<EditorList {...defaultProps} />);
      const heading = screen.getByRole('heading', { level: 2 });
      expect(heading).toHaveTextContent('Test Items');
    });

    it('displays title with custom heading level', () => {
      render(<EditorList {...defaultProps} headingLevel={3} />);
      const heading = screen.getByRole('heading', { level: 3 });
      expect(heading).toHaveTextContent('Test Items');
    });

    it('renders all items', () => {
      render(<EditorList {...defaultProps} />);
      expect(screen.getByText('Item 1')).toBeInTheDocument();
      expect(screen.getByText('Item 2')).toBeInTheDocument();
      expect(screen.getByText('Item 3')).toBeInTheDocument();
    });

    it('shows add button', () => {
      render(<EditorList {...defaultProps} />);
      expect(screen.getByRole('button', { name: /add/i })).toBeInTheDocument();
    });

    it('renders child component for each item', () => {
      render(<EditorList {...defaultProps} />);
      const editButtons = screen.getAllByText('Edit');
      const deleteButtons = screen.getAllByText('Delete');
      expect(editButtons).toHaveLength(3);
      expect(deleteButtons).toHaveLength(3);
    });
  });

  describe('Empty State', () => {
    it('displays empty message when no items', () => {
      const emptyProps = { ...defaultProps, list: [] };
      render(<EditorList {...emptyProps} />);
      expect(screen.getByText('Nothing here')).toBeInTheDocument();
    });

    it('still shows add button when empty', () => {
      const emptyProps = { ...defaultProps, list: [] };
      render(<EditorList {...emptyProps} />);
      expect(screen.getByRole('button', { name: /add/i })).toBeInTheDocument();
    });
  });

  describe('Add Functionality', () => {
    it('adds new item when add button is clicked', () => {
      render(<EditorList {...defaultProps} />);
      const addButton = screen.getByRole('button', { name: /add/i });
      
      fireEvent.click(addButton);
      
      expect(defaultProps.onChange).toHaveBeenCalledWith([
        { id: '', name: '' },
        ...defaultProps.list
      ]);
    });
  });

  describe('Edit Functionality', () => {
    it('calls onChange with modified item when edit is triggered', () => {
      render(<EditorList {...defaultProps} />);
      const editButtons = screen.getAllByText('Edit');
      
      fireEvent.click(editButtons[0]);
      
      // The mock calls onChange which triggers the parent's onChange
      expect(defaultProps.onChange).toHaveBeenCalled();
    });
  });

  describe('Delete Functionality', () => {
    it('removes item when delete is triggered', () => {
      render(<EditorList {...defaultProps} />);
      const deleteButtons = screen.getAllByText('Delete');
      
      fireEvent.click(deleteButtons[1]); // Delete second item
      
      expect(defaultProps.onChange).toHaveBeenCalledWith([
        { id: '1', name: 'Item 1' },
        { id: '3', name: 'Item 3' }
      ]);
    });

    it('handles deleting last item', () => {
      const singleItemProps = {
        ...defaultProps,
        list: [{ id: '1', name: 'Item 1' }]
      };
      render(<EditorList {...singleItemProps} />);
      const deleteButton = screen.getByText('Delete');
      
      fireEvent.click(deleteButton);
      
      expect(defaultProps.onChange).toHaveBeenCalledWith([]);
    });
  });

  describe('Heading Levels', () => {
    it('renders h1 when headingLevel is 1', () => {
      render(<EditorList {...defaultProps} headingLevel={1} />);
      const heading = screen.getByRole('heading', { level: 1 });
      expect(heading).toHaveTextContent('Test Items');
    });

    it('renders h2 when headingLevel is 2', () => {
      render(<EditorList {...defaultProps} headingLevel={2} />);
      const heading = screen.getByRole('heading', { level: 2 });
      expect(heading).toHaveTextContent('Test Items');
    });

    it('renders h3 when headingLevel is 3', () => {
      render(<EditorList {...defaultProps} headingLevel={3} />);
      const heading = screen.getByRole('heading', { level: 3 });
      expect(heading).toHaveTextContent('Test Items');
    });
  });

  describe('Child Component Props', () => {
    it('passes correct props to child components', () => {
      const ChildSpy = jest.fn(() => <div>Child</div>);
      const props = {
        ...defaultProps,
        children: <ChildSpy />
      };
      
      render(<EditorList {...props} />);
      
      // Check that child component was called with correct props
      expect(ChildSpy).toHaveBeenCalledTimes(3);
      expect(ChildSpy).toHaveBeenCalledWith(
        expect.objectContaining({
          id: 'test-list',
          index: 0,
          item: { id: '1', name: 'Item 1' },
          onChange: expect.any(Function),
          onDelete: expect.any(Function)
        }),
        {}
      );
    });
  });

  describe('Edge Cases', () => {
    it('handles empty list prop gracefully', () => {
      render(<EditorList {...defaultProps} list={[]} />);
      expect(screen.getByText('Nothing here')).toBeInTheDocument();
    });

    it('maintains item order when adding', () => {
      render(<EditorList {...defaultProps} />);
      const addButton = screen.getByRole('button', { name: /add/i });
      
      fireEvent.click(addButton);
      
      const expectedList = [
        { id: '', name: '' },
        { id: '1', name: 'Item 1' },
        { id: '2', name: 'Item 2' },
        { id: '3', name: 'Item 3' }
      ];
      expect(defaultProps.onChange).toHaveBeenCalledWith(expectedList);
    });

    it('creates copy of emptyItem to avoid mutation', () => {
      const emptyItem = { id: '', name: '', custom: 'value' };
      const props = { ...defaultProps, emptyItem };
      
      render(<EditorList {...props} />);
      const addButton = screen.getByRole('button', { name: /add/i });
      
      fireEvent.click(addButton);
      
      const callArg = defaultProps.onChange.mock.calls[0][0];
      expect(callArg[0]).toEqual(emptyItem);
      expect(callArg[0]).not.toBe(emptyItem); // Should be a copy
    });
  });
});