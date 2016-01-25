-define( OK_OR_ERROR,
         fun( { ok, _ } ) -> ok;
            ( Error ) -> Error
         end ).

-define( ID_OR_ERROR,
         fun( { ok, #{ <<"id">> := Id } } ) -> { ok, Id };
            ( Error ) -> Error
         end ).
