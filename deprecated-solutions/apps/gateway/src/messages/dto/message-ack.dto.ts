import { ApiProperty } from '@nestjs/swagger';
import { IsString, IsNumber } from 'class-validator';

export class MessageAckDto {
  @ApiProperty({
    description: 'Message identifier',
    example: 'msg_1234567890',
  })
  @IsString()
  message_id: string;

  @ApiProperty({
    description: 'Acknowledgment timestamp (milliseconds)',
    example: 1704067200000,
  })
  @IsNumber()
  ack_timestamp_ms: number;

  @ApiProperty({
    description: 'Status of message publication',
    example: 'published',
  })
  @IsString()
  status: string;
}
