import { PageHeader } from 'bootstrap-react-components';
import * as PropTypes from 'prop-types';
import React, { useState } from 'react';
import { useParams, useNavigate } from 'react-router-dom';
import { useQuery, useMutation, useQueryClient } from 'react-query';
import PlotPointForm from '../components/plotpoint/editor';
import { plotPointService } from '../services';

export default function PlotPointEdit({ id = 'PlotPointEditorPage' }) {
  const { name } = useParams();
  const navigate = useNavigate();
  const queryClient = useQueryClient();
  const [errors, setErrors] = useState([]);

  // Fetch plot point data
  const { data: plotPoint, isLoading, isError, error } = useQuery(
    ['plotPoint', name],
    () => plotPointService.getPlotPointByName(name),
    { enabled: !!name }
  );

  // Update mutation
  const updateMutation = useMutation(
    ({ id, data }) => plotPointService.updatePlotPoint(id, data),
    {
      onSuccess: () => {
        // Invalidate both the specific plot point and the list
        queryClient.invalidateQueries(['plotPoint', name]);
        queryClient.invalidateQueries(['plotPoints']);
        navigate('/');
      },
      onError: (error) => {
        const errorMessages = error.errors || [error.message || 'Failed to update plot point'];
        setErrors(Array.isArray(errorMessages) ? errorMessages : [errorMessages]);
      }
    }
  );

  const cancel = () => {
    navigate('/');
  };

  const save = async (updatedPlotPoint) => {
    if (!plotPoint?.id) {
      setErrors(['Plot point ID not found']);
      return;
    }

    // Clear previous errors
    setErrors([]);

    // Execute mutation
    updateMutation.mutate({ id: plotPoint.id, data: updatedPlotPoint });
  };

  if (isLoading) {
    return (
      <div id={id}>
        <PageHeader id={id}>
          <h1>Edit Plot Point</h1>
        </PageHeader>
        <div className="text-center mt-5">
          <div className="spinner-border" role="status">
            <span className="sr-only">Loading...</span>
          </div>
          <p className="mt-2">Loading plot point...</p>
        </div>
      </div>
    );
  }

  if (isError) {
    return (
      <div id={id}>
        <PageHeader id={id}>
          <h1>Edit Plot Point</h1>
        </PageHeader>
        <div className="alert alert-danger mt-3" role="alert">
          <h4 className="alert-heading">Error loading plot point</h4>
          <p>{error?.message || 'Plot point not found'}</p>
          <button className="btn btn-primary" onClick={() => navigate('/')}>
            Back to List
          </button>
        </div>
      </div>
    );
  }

  // Default plot point structure if data is incomplete
  const currentPlotPoint = plotPoint || {
    basicRules: {
      maximumAttributePoints: 5,
      maximumMajorHindrances: 1,
      maximumMinorHindrances: 2,
      maximumSkillPoints: 15
    },
    beasts: [],
    description: '',
    edges: [],
    gear: {
      aircraft: [],
      ammunition: [],
      armor: [],
      groundVehicles: [],
      handWeapons: [],
      mundaneItems: [],
      rangedWeapons: [],
      skills: [],
      specialWeapons: [],
      trappingsAndEffects: [],
      vehicleMountedAndAtGuns: [],
      watercraft: []
    },
    hindrances: [],
    name: '',
    powers: {
      arcaneBackgrounds: [],
    },
    races: [],
    settingRules: []
  };

  return (
    <div id={id}>
      <PageHeader id={id}>
        <h1>Edit Plot Point - {currentPlotPoint.name || name}</h1>
      </PageHeader>

      {updateMutation.isLoading && (
        <div className="alert alert-info" role="alert">
          <div className="spinner-border spinner-border-sm me-2" role="status">
            <span className="sr-only">Loading...</span>
          </div>
          Updating plot point...
        </div>
      )}

      <PlotPointForm
        id={'plotPointForm'}
        onSave={save}
        onCancel={cancel}
        onChange={save}
        plotPoint={currentPlotPoint}
        errors={errors}
        disabled={updateMutation.isLoading}
      />
    </div>
  );
}

PlotPointEdit.propTypes = {
  id: PropTypes.string
};
