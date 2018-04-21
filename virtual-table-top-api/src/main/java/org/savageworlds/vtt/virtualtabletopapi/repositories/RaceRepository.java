package org.savageworlds.vtt.virtualtabletopapi.repositories;

import org.savageworlds.vtt.virtualtabletopapi.models.Race;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

import java.util.List;

@RepositoryRestResource
public interface RaceRepository extends PagingAndSortingRepository<Race, Long> {

}
