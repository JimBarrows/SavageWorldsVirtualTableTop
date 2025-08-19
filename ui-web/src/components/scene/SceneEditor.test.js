import React from 'react'
import { render, screen, fireEvent, waitFor } from '@testing-library/react'
import SceneEditor from './SceneEditor'

describe('SceneEditor', () => {
  const mockScene = {
    id: '1',
    name: 'Opening Tavern Scene',
    description: 'The heroes gather at the Red Dragon Inn',
    dramatis_personae: [
      { name: 'Sir Gareth', description: 'A noble knight' },
      { name: 'Mara', description: 'A cunning thief' }
    ],
    places: [
      { name: 'Red Dragon Inn', description: 'A cozy tavern' },
      { name: 'Main Hall', description: 'The central dining area' }
    ]
  }

  const mockAvailableCharacters = [
    { name: 'Sir Gareth', description: 'A noble knight' },
    { name: 'Mara', description: 'A cunning thief' },
    { name: 'Grimjaw', description: 'An orc bartender' },
    { name: 'Eldara', description: 'An elven mage' }
  ]

  const mockAvailablePlaces = [
    { name: 'Red Dragon Inn', description: 'A cozy tavern' },
    { name: 'Main Hall', description: 'The central dining area' },
    { name: 'Private Room', description: 'A small room for intimate conversations' },
    { name: 'Kitchen', description: 'The tavern\'s busy kitchen' }
  ]

  describe('rendering', () => {
    test('should render scene editor form', () => {
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} availablePlaces={mockAvailablePlaces} />)
      
      expect(screen.getByDisplayValue('Opening Tavern Scene')).toBeInTheDocument()
      expect(screen.getByDisplayValue('The heroes gather at the Red Dragon Inn')).toBeInTheDocument()
      expect(screen.getByText('Save Scene')).toBeInTheDocument()
    })

    test('should render empty form for new scene', () => {
      const newScene = { id: '', name: '', description: '', dramatis_personae: [], places: [] }
      render(<SceneEditor scene={newScene} availableCharacters={mockAvailableCharacters} availablePlaces={mockAvailablePlaces} />)
      
      expect(screen.getByPlaceholderText('Scene Name')).toHaveValue('')
      expect(screen.getByPlaceholderText('Scene Description')).toHaveValue('')
    })

    test('should render dramatis personae list', () => {
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} availablePlaces={mockAvailablePlaces} />)
      
      expect(screen.getByText('Dramatis Personae')).toBeInTheDocument()
      expect(screen.getAllByText('Sir Gareth')).toHaveLength(2) // One in table, one in dropdown
      expect(screen.getAllByText('Mara')).toHaveLength(2) // One in table, one in dropdown
    })

    test('should render scene places list', () => {
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} availablePlaces={mockAvailablePlaces} />)
      
      expect(screen.getByText('Scene Places')).toBeInTheDocument()
      expect(screen.getAllByText('Red Dragon Inn')).toHaveLength(2) // One in table, one in dropdown
      expect(screen.getAllByText('Main Hall')).toHaveLength(2) // One in table, one in dropdown
    })

    test('should render character and place selection dropdowns', () => {
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} availablePlaces={mockAvailablePlaces} />)
      
      expect(screen.getByText('Add Character')).toBeInTheDocument()
      expect(screen.getByText('Add Place')).toBeInTheDocument()
      expect(screen.getAllByRole('combobox')).toHaveLength(2)
    })
  })

  describe('form interactions', () => {
    test('should update scene name when input changes', async () => {
      const onChange = jest.fn()
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} availablePlaces={mockAvailablePlaces} onChange={onChange} />)
      
      const nameInput = screen.getByDisplayValue('Opening Tavern Scene')
      fireEvent.change(nameInput, { target: { value: 'Updated Scene Name' } })
      
      await waitFor(() => {
        expect(onChange).toHaveBeenCalledWith(expect.objectContaining({
          name: 'Updated Scene Name'
        }))
      })
    })

    test('should update scene description when textarea changes', async () => {
      const onChange = jest.fn()
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} availablePlaces={mockAvailablePlaces} onChange={onChange} />)
      
      const descriptionTextarea = screen.getByDisplayValue('The heroes gather at the Red Dragon Inn')
      fireEvent.change(descriptionTextarea, { target: { value: 'Updated description' } })
      
      await waitFor(() => {
        expect(onChange).toHaveBeenCalledWith(expect.objectContaining({
          description: 'Updated description'
        }))
      })
    })

    test('should call onSave when save button clicked', () => {
      const onSave = jest.fn()
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} availablePlaces={mockAvailablePlaces} onSave={onSave} />)
      
      const saveButton = screen.getByText('Save Scene')
      fireEvent.click(saveButton)
      
      expect(onSave).toHaveBeenCalledWith(mockScene)
    })
  })

  describe('character management', () => {
    test('should add character to dramatis personae', async () => {
      const onChange = jest.fn()
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} availablePlaces={mockAvailablePlaces} onChange={onChange} />)
      
      const selects = screen.getAllByRole('combobox')
      const characterSelect = selects[0] // First dropdown is for characters
      fireEvent.change(characterSelect, { target: { value: 'Grimjaw' } })
      
      const addButton = screen.getByText('Add Character')
      fireEvent.click(addButton)
      
      await waitFor(() => {
        expect(onChange).toHaveBeenCalledWith(expect.objectContaining({
          dramatis_personae: expect.arrayContaining([
            expect.objectContaining({ name: 'Grimjaw' })
          ])
        }))
      })
    })

    test('should remove character from dramatis personae', async () => {
      const onChange = jest.fn()
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} availablePlaces={mockAvailablePlaces} onChange={onChange} />)
      
      const removeButtons = screen.getAllByText('Remove')
      // Find the remove button in the character section (first table)
      fireEvent.click(removeButtons[0])
      
      await waitFor(() => {
        expect(onChange).toHaveBeenCalledWith(expect.objectContaining({
          dramatis_personae: expect.not.arrayContaining([
            expect.objectContaining({ name: 'Sir Gareth' })
          ])
        }))
      })
    })

    test('should update character description', async () => {
      const onChange = jest.fn()
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} availablePlaces={mockAvailablePlaces} onChange={onChange} />)
      
      const editButtons = screen.getAllByText('Edit')
      // Find the edit button in the character section (first table)
      fireEvent.click(editButtons[0])
      
      const descriptionInput = screen.getByDisplayValue('A noble knight')
      fireEvent.change(descriptionInput, { target: { value: 'The party leader' } })
      
      const saveCharacterButton = screen.getByText('Save Character')
      fireEvent.click(saveCharacterButton)
      
      await waitFor(() => {
        expect(onChange).toHaveBeenCalledWith(expect.objectContaining({
          dramatis_personae: expect.arrayContaining([
            expect.objectContaining({ 
              name: 'Sir Gareth',
              description: 'The party leader'
            })
          ])
        }))
      })
    })
  })

  describe('place management', () => {
    test('should add place to scene places', async () => {
      const onChange = jest.fn()
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} availablePlaces={mockAvailablePlaces} onChange={onChange} />)
      
      const selects = screen.getAllByRole('combobox')
      const placeSelect = selects[1] // Second dropdown is for places
      fireEvent.change(placeSelect, { target: { value: 'Kitchen' } })
      
      const addButton = screen.getByText('Add Place')
      fireEvent.click(addButton)
      
      await waitFor(() => {
        expect(onChange).toHaveBeenCalledWith(expect.objectContaining({
          places: expect.arrayContaining([
            expect.objectContaining({ name: 'Kitchen' })
          ])
        }))
      })
    })

    test('should remove place from scene places', async () => {
      const onChange = jest.fn()
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} availablePlaces={mockAvailablePlaces} onChange={onChange} />)
      
      const removeButtons = screen.getAllByText('Remove')
      // Find the remove button in the places section (should be after character buttons)
      const placeRemoveButton = removeButtons[removeButtons.length - 1] // Last remove button should be for places
      fireEvent.click(placeRemoveButton)
      
      await waitFor(() => {
        expect(onChange).toHaveBeenCalledWith(expect.objectContaining({
          places: expect.not.arrayContaining([
            expect.objectContaining({ name: 'Main Hall' })
          ])
        }))
      })
    })

    test('should update place description', async () => {
      const onChange = jest.fn()
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} availablePlaces={mockAvailablePlaces} onChange={onChange} />)
      
      const editButtons = screen.getAllByText('Edit')
      // Find the edit button in the places section (should be after character buttons)
      const placeEditButton = editButtons[editButtons.length - 1] // Last edit button should be for places
      fireEvent.click(placeEditButton)
      
      const descriptionInput = screen.getByDisplayValue('The central dining area')
      fireEvent.change(descriptionInput, { target: { value: 'The bustling main hall' } })
      
      const savePlaceButton = screen.getByText('Save Place')
      fireEvent.click(savePlaceButton)
      
      await waitFor(() => {
        expect(onChange).toHaveBeenCalledWith(expect.objectContaining({
          places: expect.arrayContaining([
            expect.objectContaining({ 
              name: 'Main Hall',
              description: 'The bustling main hall'
            })
          ])
        }))
      })
    })
  })

  describe('validation', () => {
    test('should show error for empty scene name', () => {
      const sceneWithoutName = { ...mockScene, name: '' }
      render(<SceneEditor scene={sceneWithoutName} availableCharacters={mockAvailableCharacters} availablePlaces={mockAvailablePlaces} />)
      
      const saveButton = screen.getByText('Save Scene')
      fireEvent.click(saveButton)
      
      expect(screen.getByText('Scene name is required')).toBeInTheDocument()
    })

    test('should prevent adding duplicate characters', () => {
      const onChange = jest.fn()
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} availablePlaces={mockAvailablePlaces} onChange={onChange} />)
      
      const selects = screen.getAllByRole('combobox')
      const characterSelect = selects[0]
      fireEvent.change(characterSelect, { target: { value: 'Sir Gareth' } })
      
      const addButton = screen.getByText('Add Character')
      fireEvent.click(addButton)
      
      expect(screen.getByText('Character is already in the scene')).toBeInTheDocument()
      expect(onChange).not.toHaveBeenCalled()
    })

    test('should prevent adding duplicate places', () => {
      const onChange = jest.fn()
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} availablePlaces={mockAvailablePlaces} onChange={onChange} />)
      
      const selects = screen.getAllByRole('combobox')
      const placeSelect = selects[1]
      fireEvent.change(placeSelect, { target: { value: 'Red Dragon Inn' } })
      
      const addButton = screen.getByText('Add Place')
      fireEvent.click(addButton)
      
      expect(screen.getByText('Place is already in the scene')).toBeInTheDocument()
      expect(onChange).not.toHaveBeenCalled()
    })
  })

  describe('error handling', () => {
    test('should handle missing availableCharacters prop', () => {
      render(<SceneEditor scene={mockScene} availablePlaces={mockAvailablePlaces} />)
      
      expect(screen.getByDisplayValue('Opening Tavern Scene')).toBeInTheDocument()
    })

    test('should handle missing availablePlaces prop', () => {
      render(<SceneEditor scene={mockScene} availableCharacters={mockAvailableCharacters} />)
      
      expect(screen.getByDisplayValue('Opening Tavern Scene')).toBeInTheDocument()
    })

    test('should handle null scene prop', () => {
      render(<SceneEditor scene={null} availableCharacters={mockAvailableCharacters} availablePlaces={mockAvailablePlaces} />)
      
      // Should render with default empty scene
      expect(screen.getByPlaceholderText('Scene Name')).toBeInTheDocument()
    })
  })
})