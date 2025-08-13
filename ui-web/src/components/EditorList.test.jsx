import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import EditorList from './EditorList';

describe('EditorList Component', () => {
  const defaultProps = {
    items: [
      { id: '1', name: 'Item 1', description: 'Description 1' },
      { id: '2', name: 'Item 2', description: 'Description 2' },
      { id: '3', name: 'Item 3', description: 'Description 3' }
    ],
    onEdit: jest.fn(),
    onDelete: jest.fn(),
    onAdd: jest.fn(),
    title: 'Test Items',
    itemLabel: 'item'
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders without crashing', () => {
      render(<EditorList {...defaultProps} />);
    });

    it('displays the title', () => {
      render(<EditorList {...defaultProps} />);
      expect(screen.getByText('Test Items')).toBeInTheDocument();
    });

    it('renders all items', () => {
      render(<EditorList {...defaultProps} />);
      expect(screen.getByText('Item 1')).toBeInTheDocument();
      expect(screen.getByText('Item 2')).toBeInTheDocument();
      expect(screen.getByText('Item 3')).toBeInTheDocument();
    });

    it('displays item descriptions', () => {
      render(<EditorList {...defaultProps} />);
      expect(screen.getByText('Description 1')).toBeInTheDocument();
      expect(screen.getByText('Description 2')).toBeInTheDocument();
    });

    it('shows add button', () => {
      render(<EditorList {...defaultProps} />);
      expect(screen.getByRole('button', { name: /add.*item/i })).toBeInTheDocument();
    });

    it('renders edit and delete buttons for each item', () => {
      render(<EditorList {...defaultProps} />);
      const editButtons = screen.getAllByRole('button', { name: /edit/i });
      const deleteButtons = screen.getAllByRole('button', { name: /delete/i });
      
      expect(editButtons).toHaveLength(3);
      expect(deleteButtons).toHaveLength(3);
    });
  });

  describe('Empty State', () => {
    it('displays empty message when no items', () => {
      render(<EditorList {...defaultProps} items={[]} />);
      expect(screen.getByText(/no items/i)).toBeInTheDocument();
    });

    it('shows custom empty message', () => {
      render(
        <EditorList 
          {...defaultProps} 
          items={[]} 
          emptyMessage="No data available"
        />
      );
      expect(screen.getByText('No data available')).toBeInTheDocument();
    });

    it('still shows add button when empty', () => {
      render(<EditorList {...defaultProps} items={[]} />);
      expect(screen.getByRole('button', { name: /add/i })).toBeInTheDocument();
    });
  });

  describe('Add Functionality', () => {
    it('calls onAdd when add button is clicked', () => {
      render(<EditorList {...defaultProps} />);
      const addButton = screen.getByRole('button', { name: /add.*item/i });
      
      fireEvent.click(addButton);
      expect(defaultProps.onAdd).toHaveBeenCalledTimes(1);
    });

    it('hides add button when canAdd is false', () => {
      render(<EditorList {...defaultProps} canAdd={false} />);
      expect(screen.queryByRole('button', { name: /add/i })).not.toBeInTheDocument();
    });

    it('disables add button when addDisabled is true', () => {
      render(<EditorList {...defaultProps} addDisabled />);
      const addButton = screen.getByRole('button', { name: /add/i });
      expect(addButton).toBeDisabled();
    });
  });

  describe('Edit Functionality', () => {
    it('calls onEdit with correct item when edit is clicked', () => {
      render(<EditorList {...defaultProps} />);
      const editButtons = screen.getAllByRole('button', { name: /edit/i });
      
      fireEvent.click(editButtons[0]);
      expect(defaultProps.onEdit).toHaveBeenCalledWith(defaultProps.items[0]);
    });

    it('hides edit buttons when canEdit is false', () => {
      render(<EditorList {...defaultProps} canEdit={false} />);
      expect(screen.queryAllByRole('button', { name: /edit/i })).toHaveLength(0);
    });

    it('disables edit buttons when editDisabled is true', () => {
      render(<EditorList {...defaultProps} editDisabled />);
      const editButtons = screen.getAllByRole('button', { name: /edit/i });
      editButtons.forEach(button => {
        expect(button).toBeDisabled();
      });
    });
  });

  describe('Delete Functionality', () => {
    it('shows confirmation dialog before delete', () => {
      render(<EditorList {...defaultProps} />);
      const deleteButtons = screen.getAllByRole('button', { name: /delete/i });
      
      fireEvent.click(deleteButtons[0]);
      expect(screen.getByText(/are you sure/i)).toBeInTheDocument();
    });

    it('calls onDelete when delete is confirmed', () => {
      render(<EditorList {...defaultProps} />);
      const deleteButtons = screen.getAllByRole('button', { name: /delete/i });
      
      fireEvent.click(deleteButtons[0]);
      const confirmButton = screen.getByRole('button', { name: /confirm/i });
      fireEvent.click(confirmButton);
      
      expect(defaultProps.onDelete).toHaveBeenCalledWith(defaultProps.items[0]);
    });

    it('cancels delete when cancel is clicked', () => {
      render(<EditorList {...defaultProps} />);
      const deleteButtons = screen.getAllByRole('button', { name: /delete/i });
      
      fireEvent.click(deleteButtons[0]);
      const cancelButton = screen.getByRole('button', { name: /cancel/i });
      fireEvent.click(cancelButton);
      
      expect(defaultProps.onDelete).not.toHaveBeenCalled();
      expect(screen.queryByText(/are you sure/i)).not.toBeInTheDocument();
    });

    it('hides delete buttons when canDelete is false', () => {
      render(<EditorList {...defaultProps} canDelete={false} />);
      expect(screen.queryAllByRole('button', { name: /delete/i })).toHaveLength(0);
    });

    it('skips confirmation when confirmDelete is false', () => {
      render(<EditorList {...defaultProps} confirmDelete={false} />);
      const deleteButtons = screen.getAllByRole('button', { name: /delete/i });
      
      fireEvent.click(deleteButtons[0]);
      expect(screen.queryByText(/are you sure/i)).not.toBeInTheDocument();
      expect(defaultProps.onDelete).toHaveBeenCalledWith(defaultProps.items[0]);
    });
  });

  describe('Filtering and Searching', () => {
    it('displays search input when searchable is true', () => {
      render(<EditorList {...defaultProps} searchable />);
      expect(screen.getByPlaceholderText(/search/i)).toBeInTheDocument();
    });

    it('filters items based on search term', () => {
      render(<EditorList {...defaultProps} searchable />);
      const searchInput = screen.getByPlaceholderText(/search/i);
      
      fireEvent.change(searchInput, { target: { value: 'Item 1' } });
      
      expect(screen.getByText('Item 1')).toBeInTheDocument();
      expect(screen.queryByText('Item 2')).not.toBeInTheDocument();
      expect(screen.queryByText('Item 3')).not.toBeInTheDocument();
    });

    it('searches in descriptions when searchInDescription is true', () => {
      render(<EditorList {...defaultProps} searchable searchInDescription />);
      const searchInput = screen.getByPlaceholderText(/search/i);
      
      fireEvent.change(searchInput, { target: { value: 'Description 2' } });
      
      expect(screen.queryByText('Item 1')).not.toBeInTheDocument();
      expect(screen.getByText('Item 2')).toBeInTheDocument();
    });

    it('shows no results message when search yields nothing', () => {
      render(<EditorList {...defaultProps} searchable />);
      const searchInput = screen.getByPlaceholderText(/search/i);
      
      fireEvent.change(searchInput, { target: { value: 'nonexistent' } });
      
      expect(screen.getByText(/no items found/i)).toBeInTheDocument();
    });
  });

  describe('Sorting', () => {
    it('displays sort options when sortable is true', () => {
      render(<EditorList {...defaultProps} sortable />);
      expect(screen.getByLabelText(/sort/i)).toBeInTheDocument();
    });

    it('sorts items alphabetically by name', () => {
      const unsortedItems = [
        { id: '1', name: 'Zebra' },
        { id: '2', name: 'Apple' },
        { id: '3', name: 'Banana' }
      ];
      
      render(<EditorList {...defaultProps} items={unsortedItems} sortable />);
      const sortSelect = screen.getByLabelText(/sort/i);
      
      fireEvent.change(sortSelect, { target: { value: 'name-asc' } });
      
      const itemElements = screen.getAllByTestId('list-item');
      expect(itemElements[0]).toHaveTextContent('Apple');
      expect(itemElements[1]).toHaveTextContent('Banana');
      expect(itemElements[2]).toHaveTextContent('Zebra');
    });

    it('sorts items in reverse alphabetical order', () => {
      render(<EditorList {...defaultProps} sortable />);
      const sortSelect = screen.getByLabelText(/sort/i);
      
      fireEvent.change(sortSelect, { target: { value: 'name-desc' } });
      
      const itemElements = screen.getAllByTestId('list-item');
      expect(itemElements[0]).toHaveTextContent('Item 3');
      expect(itemElements[2]).toHaveTextContent('Item 1');
    });
  });

  describe('Selection', () => {
    it('allows item selection when selectable is true', () => {
      render(<EditorList {...defaultProps} selectable />);
      const checkboxes = screen.getAllByRole('checkbox');
      expect(checkboxes).toHaveLength(3);
    });

    it('calls onSelectionChange when items are selected', () => {
      const onSelectionChange = jest.fn();
      render(
        <EditorList 
          {...defaultProps} 
          selectable 
          onSelectionChange={onSelectionChange}
        />
      );
      
      const checkboxes = screen.getAllByRole('checkbox');
      fireEvent.click(checkboxes[0]);
      
      expect(onSelectionChange).toHaveBeenCalledWith([defaultProps.items[0]]);
    });

    it('supports select all functionality', () => {
      const onSelectionChange = jest.fn();
      render(
        <EditorList 
          {...defaultProps} 
          selectable 
          selectAll 
          onSelectionChange={onSelectionChange}
        />
      );
      
      const selectAllCheckbox = screen.getByLabelText(/select all/i);
      fireEvent.click(selectAllCheckbox);
      
      expect(onSelectionChange).toHaveBeenCalledWith(defaultProps.items);
    });
  });

  describe('Loading State', () => {
    it('displays loading indicator when loading is true', () => {
      render(<EditorList {...defaultProps} loading />);
      expect(screen.getByText(/loading/i)).toBeInTheDocument();
    });

    it('hides items when loading', () => {
      render(<EditorList {...defaultProps} loading />);
      expect(screen.queryByText('Item 1')).not.toBeInTheDocument();
    });
  });

  describe('Custom Rendering', () => {
    it('uses custom item renderer when provided', () => {
      const customRenderer = jest.fn(item => <div>Custom: {item.name}</div>);
      render(<EditorList {...defaultProps} renderItem={customRenderer} />);
      
      expect(screen.getByText('Custom: Item 1')).toBeInTheDocument();
      expect(customRenderer).toHaveBeenCalledTimes(3);
    });

    it('uses custom action renderer', () => {
      const customActions = jest.fn(item => (
        <button>Custom Action for {item.name}</button>
      ));
      render(<EditorList {...defaultProps} renderActions={customActions} />);
      
      expect(screen.getByText('Custom Action for Item 1')).toBeInTheDocument();
    });
  });

  describe('Accessibility', () => {
    it('has proper list semantics', () => {
      render(<EditorList {...defaultProps} />);
      const list = screen.getByRole('list');
      expect(list).toBeInTheDocument();
      
      const listItems = screen.getAllByRole('listitem');
      expect(listItems).toHaveLength(3);
    });

    it('has accessible labels for actions', () => {
      render(<EditorList {...defaultProps} />);
      const editButtons = screen.getAllByRole('button', { name: /edit/i });
      editButtons.forEach((button, index) => {
        expect(button).toHaveAttribute('aria-label', expect.stringContaining('Edit'));
      });
    });

    it('announces list updates to screen readers', () => {
      const { rerender } = render(<EditorList {...defaultProps} />);
      
      const newItems = [...defaultProps.items, { id: '4', name: 'Item 4' }];
      rerender(<EditorList {...defaultProps} items={newItems} />);
      
      expect(screen.getByText('Item 4')).toBeInTheDocument();
    });

    it('provides keyboard navigation support', () => {
      render(<EditorList {...defaultProps} />);
      const firstEditButton = screen.getAllByRole('button', { name: /edit/i })[0];
      
      firstEditButton.focus();
      expect(document.activeElement).toBe(firstEditButton);
    });
  });
});