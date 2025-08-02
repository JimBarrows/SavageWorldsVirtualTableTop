import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import RememberMe from './RememberMe';

describe('RememberMe Component', () => {
  it('renders checkbox with correct label', () => {
    render(<RememberMe checked={false} onChange={() => {}} />);
    
    const checkbox = screen.getByRole('checkbox');
    const label = screen.getByText('Remember Me');
    
    expect(checkbox).toBeInTheDocument();
    expect(label).toBeInTheDocument();
  });

  it('displays unchecked by default', () => {
    render(<RememberMe checked={false} onChange={() => {}} />);
    
    const checkbox = screen.getByRole('checkbox');
    expect(checkbox).not.toBeChecked();
  });

  it('displays checked when prop is true', () => {
    render(<RememberMe checked={true} onChange={() => {}} />);
    
    const checkbox = screen.getByRole('checkbox');
    expect(checkbox).toBeChecked();
  });

  it('calls onChange when clicked', () => {
    const mockOnChange = jest.fn();
    render(<RememberMe checked={false} onChange={mockOnChange} />);
    
    const checkbox = screen.getByRole('checkbox');
    fireEvent.click(checkbox);
    
    expect(mockOnChange).toHaveBeenCalledTimes(1);
    expect(mockOnChange).toHaveBeenCalledWith(true);
  });

  it('calls onChange with false when unchecking', () => {
    const mockOnChange = jest.fn();
    render(<RememberMe checked={true} onChange={mockOnChange} />);
    
    const checkbox = screen.getByRole('checkbox');
    fireEvent.click(checkbox);
    
    expect(mockOnChange).toHaveBeenCalledTimes(1);
    expect(mockOnChange).toHaveBeenCalledWith(false);
  });

  it('has correct accessibility attributes', () => {
    render(<RememberMe checked={false} onChange={() => {}} />);
    
    const checkbox = screen.getByRole('checkbox');
    expect(checkbox).toHaveAttribute('id', 'rememberMe');
    expect(checkbox).toHaveAttribute('aria-label', 'Remember Me');
  });
});